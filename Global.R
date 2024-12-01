# Functions to use

hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}
calculate_hex_coords = function(shots, binwidths) {
  xbnds = hex_bounds(shots$coord_x_adj, binwidths[1])
  xbins = diff(xbnds) / binwidths[1]
  ybnds = hex_bounds(shots$coord_y_adj, binwidths[2])
  ybins = diff(ybnds) / binwidths[2]
  
  hb = hexbin(
    x = shots$coord_x_adj,
    y = shots$coord_y_adj,
    xbins = xbins,
    xbnds = xbnds,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
  )
  
  shots = mutate(shots, hexbin_id = hb@cID)
  
  hexbin_stats = shots %>%
    group_by(hexbin_id) %>%
    summarize(
      hex_attempts = n(),
      hex_pct = mean(FGM_FLAG),
      hex_points_scored = sum(FGM_FLAG * as.integer(shot_value)),
      hex_points_per_shot = mean(FGM_FLAG * as.integer(shot_value))
    ) 
  
  hexbin_ids_to_zones = shots %>%
    group_by(hexbin_id, SHOT_ZONE_DIST, SHOT_ZONE_AREA_2) %>%
    summarize(attempts = n()) %>%
    ungroup() %>%
    arrange(hexbin_id, desc(attempts)) %>%
    group_by(hexbin_id) %>%
    filter(row_number() == 1) %>%
    select(hexbin_id, SHOT_ZONE_DIST, SHOT_ZONE_AREA_2)
  
  hexbin_stats = inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")
  
  # from hexbin package, see: https://github.com/edzer/hexbin
  sx = hb@xbins / diff(hb@xbnds)
  sy = (hb@xbins * hb@shape) / diff(hb@ybnds)
  dx = 1 / (2 * sx)
  dy = 1 / (2 * sqrt(3) * sy)
  origin_coords = hexcoords(dx, dy)
  
  hex_centers = hcell2xy(hb)
  
  hexbin_coords = bind_rows(lapply(1:hb@ncells, function(i) {
    data.frame(
      x = origin_coords$x + hex_centers$x[i],
      y = origin_coords$y + hex_centers$y[i],
      center_x = hex_centers$x[i],
      center_y = hex_centers$y[i],
      hexbin_id = hb@cell[i]
    )
  }))
  
  inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")
}
calculate_hexbins_from_shots = function(shots, league_averages, binwidths = c(0.6, 0.6), min_radius_factor = 0.03, fg_diff_limits = c(-0.15, 0.15), fg_pct_limits = c(0.2, 0.7), pps_limits = c(0.5, 1.5)) {
  if (nrow(shots) == 0) {
    return(list())
  }
  
  grouped_shots = group_by(shots, SHOT_ZONE_DIST, SHOT_ZONE_AREA_2)
  
  zone_stats = grouped_shots %>%
    summarize(
      zone_attempts = n(),
      zone_pct = mean(FGM_FLAG),
      zone_points_scored = sum(FGM_FLAG * as.numeric(shot_value)),
      zone_points_per_shot = mean(FGM_FLAG * as.numeric(shot_value))
    )
  
  league_zone_stats = league_averages %>%
    group_by(SHOT_ZONE_DIST, SHOT_ZONE_AREA_2) %>%
    summarize(league_pct = sum(fgm) / sum(fga))
  
  hex_data = calculate_hex_coords(shots, binwidths = binwidths)
  
  join_keys = c("SHOT_ZONE_AREA_2", "SHOT_ZONE_DIST")
  
  hex_data = hex_data %>%
    inner_join(zone_stats, by = join_keys) %>%
    inner_join(league_zone_stats, by = join_keys)
  
  max_hex_attempts = max(hex_data$hex_attempts)
  
  
  hex_data = mutate(hex_data,
                    radius_factor = min_radius_factor + (1 - min_radius_factor) * log(hex_attempts + 1) / log(max_hex_attempts + 1),
                    adj_x = center_x + radius_factor * (x - center_x),
                    adj_y = center_y + radius_factor * (y - center_y),
                    bounded_fg_diff = pmin(pmax(zone_pct - league_pct, fg_diff_limits[1]), fg_diff_limits[2]),
                    bounded_fg_pct = pmin(pmax(zone_pct, fg_pct_limits[1]), fg_pct_limits[2]),
                    bounded_points_per_shot = pmin(pmax(zone_points_per_shot, pps_limits[1]), pps_limits[2]))
  
  list(hex_data = hex_data, fg_diff_limits = fg_diff_limits, fg_pct_limits = fg_pct_limits, pps_limits = pps_limits)
  
}

construct_court <- function() {
  outer_lines <- data.frame(
    x = c(-7.5, -7.5, 7.5, 7.5, -7.5),
    y = c(0, 14, 14, 0, 0),
    type = "Outer lines"
  )
  
  paint <- data.frame(
    x = c(-2.45, -2.45, 2.45, 2.45),
    y = c(0, 5.8, 5.8, 0),
    type = "Paint"
  )
  
  ft_circle <- data.frame(
    construct_arc(x0 = 0, y0 = 5.8, r = 1.8, start = 0, stop = pi),
    type = "FT circle"
  )
  
  # The 3pt line transforms into a straight line in the corners
  # Precisely, it transforms to a vertical line when the x coordinates
  # of the arc are above or below 6.6 and -6.6 respectively.
  upper_arc3 <- data.frame(
    construct_arc(x0 = 0, y0 = 1.575, r = 6.75, start = 0, stop = pi),
    type = "Upper arc"
  ) %>%
    dplyr::filter(abs(.data$x) <= 6.6)
  
  # To find the y coordinate where the vertical line in the corner and
  # the 3pt arc meet, we just find the minimum value of the arc above
  y_max_corner <- min(upper_arc3$y)
  left_corner3 <- data.frame(
    x = c(-6.6, -6.6),
    y = c(0, y_max_corner),
    type = "Left corner 3"
  )
  right_corner3 <- data.frame(
    x = c(6.6, 6.6),
    y = c(y_max_corner, 0),
    type = "Right corner 3"
  )
  arc3 <- rbind(right_corner3, upper_arc3, left_corner3)
  
  backboard <- data.frame(
    x = c(-0.9, 0.9),
    y = c(1.2, 1.2),
    type = "backboard"
  )
  
  rim <- construct_arc(x0 = 0, y0 = 1.575, r = 0.225,
                       start = 0, stop = 2 * pi)
  
  semi_circle <- data.frame(
    construct_arc(0, 1.575, r = 1.25, 0, pi),
    type = "semi_circle"
  )
  semi_circle_left <- data.frame(
    x = c(-1.25, -1.25),
    y = c(1.575, 1.2),
    type = "semi_circle_left"
  )
  semi_circle_right <- data.frame(
    x = c(1.25, 1.25),
    y = c(1.575, 1.2),
    type = "semi_circle_right"
  )
  restricted_area <- rbind(semi_circle_right, semi_circle, semi_circle_left)
  
  middle_circle <- data.frame(
    construct_arc(0, 14, 1.8, pi, 2 * pi),
    type = "middle_circle"
  )
  
  court <- list(
    outer_lines = outer_lines,
    paint = paint,
    ft_circle = ft_circle,
    arc3 = arc3,
    backboard = backboard,
    rim = rim,
    restricted_area = restricted_area,
    middle_circle = middle_circle
  )
  
  court
}
construct_arc <- function(x0, y0, r, start, stop) {
  by <- ifelse(start <= stop, 0.001, -0.001)
  theta <- seq(start, stop, by)
  x <- x0 + r * cos(theta)
  y <- y0 + r * sin(theta)
  data.frame(x, y)
}

plotShothexbinLA <- function(shots) {
  court <- construct_court()
  ggplot() +
    geom_polygon(
      data = shots,
      aes(
        x = adj_x,
        y = adj_y,
        group = hexbin_id, 
        fill = bounded_fg_diff) # bounded points per shot, bounded_fg_diff
    ) +
    geom_path(data = court$outer_lines, aes(x,  y)) +
    geom_path(data = court$paint, aes(x,  y)) +
    geom_path(data = court$arc3, aes(x,  y)) +
    geom_path(data = court$ft_circle, aes(x, y)) +
    geom_path(data = court$backboard, aes(x, y)) +
    geom_path(data = court$rim, aes(x, y)) +
    geom_path(data = court$restricted_area, aes(x, y)) +
    geom_path(data = court$middle_circle, aes(x, y)) +
    coord_fixed(xlim = c(-7.8, 7.8), ylim = c(0, 14)) +
    # coord_cartesian(xlim = c(-7.8, 7.8), ylim = c(0, 14)) +
    ggplot2::theme_void() +
    paletteer::scale_fill_paletteer_c("viridis::turbo",
                                      direction = 1, # -1
                                      limits = c(-.15, .15),
                                      breaks = seq(-.15, .15, .03),
                                      labels = c("-15%", "-12%", "-9%", "-6%", "-3%", "0%", "+3%", "+6%", "+9%", "+12%", "+15%"),
                                      "FG % vs. League Average"
    ) +
    guides(fill=guide_legend(
      label.position = 'bottom', 
      title.position = 'top', 
      keywidth=.45,
      keyheight=.15, 
      default.unit="inch", 
      title.hjust = .5,
      title.vjust = -.5,
      label.vjust = 3,
      nrow = 1))  +
    theme(text=element_text(size=20,  family="Arial"), 
          legend.spacing.x = unit(0, 'cm'), 
          legend.title=element_text(size=16), 
          legend.text = element_text(size = rel(0.6)), 
          legend.margin=margin(-10,0,-1,0),
          legend.position = 'bottom',
          legend.box.margin=margin(-30,0,15,0), 
          plot.title = element_text(hjust = 0.5, vjust = -6, size = 23, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 15, vjust = -9), 
          plot.caption = element_text(face = "italic", size = 13), 
          plot.margin = margin(0, -5, 0, -5, "cm"))
}
plotShothexbinTA <- function(shots) {
  court <- construct_court()
  ggplot() +
    geom_polygon(
      data = shots,
      aes(
        x = adj_x,
        y = adj_y,
        group = hexbin_id, 
        fill = bounded_fg_diff) # bounded points per shot, bounded_fg_diff
    ) +
    geom_path(data = court$outer_lines, aes(x,  y)) +
    geom_path(data = court$paint, aes(x,  y)) +
    geom_path(data = court$arc3, aes(x,  y)) +
    geom_path(data = court$ft_circle, aes(x, y)) +
    geom_path(data = court$backboard, aes(x, y)) +
    geom_path(data = court$rim, aes(x, y)) +
    geom_path(data = court$restricted_area, aes(x, y)) +
    geom_path(data = court$middle_circle, aes(x, y)) +
    coord_fixed(xlim = c(-7.8, 7.8), ylim = c(0, 14)) +
    # coord_cartesian(xlim = c(-7.8, 7.8), ylim = c(0, 14)) +
    ggplot2::theme_void() +
    scale_fill_distiller(direction = -1, # -1,
                         palette = 'RdYlBu',
                         limits = c(-.15, .15),
                         breaks = seq(-.15, .15, .03),
                         labels = c("-15%", "-12%", "-9%", "-6%", "-3%", "0%", "+3%", "+6%", "+9%", "+12%", "+15%"),
                         'FG % vs. Team Average'
    ) +
    guides(fill=guide_legend(
      label.position = 'bottom', 
      title.position = 'top', 
      keywidth=.45,
      keyheight=.15, 
      default.unit="inch", 
      title.hjust = .5,
      title.vjust = -1,
      label.vjust = 3,
      nrow = 1))  +
    theme(text=element_text(size=20,  family="Arial"), # Gill Sans MT), 
          legend.spacing.x = unit(0, 'cm'), 
          legend.title=element_text(size=16), 
          legend.text = element_text(size = rel(0.6)), 
          legend.margin=margin(-10,0,-1,0),
          legend.position = 'bottom',
          legend.box.margin=margin(-30,0,15,0), 
          plot.title = element_text(hjust = 0.5, vjust = -6, size = 23, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 15, vjust = -9), 
          plot.caption = element_text(face = "italic", size = 13), 
          plot.margin = margin(0, -5, 0, -5, "cm"))
}
plotShothexbinPA <- function(shots) {
  court <- construct_court()
  ggplot() +
    geom_polygon(
      data = shots,
      aes(
        x = adj_x,
        y = adj_y,
        group = hexbin_id, 
        fill = bounded_fg_diff) # bounded points per shot, bounded_fg_diff
    ) +
    geom_path(data = court$outer_lines, aes(x,  y)) +
    geom_path(data = court$paint, aes(x,  y)) +
    geom_path(data = court$arc3, aes(x,  y)) +
    geom_path(data = court$ft_circle, aes(x, y)) +
    geom_path(data = court$backboard, aes(x, y)) +
    geom_path(data = court$rim, aes(x, y)) +
    geom_path(data = court$restricted_area, aes(x, y)) +
    geom_path(data = court$middle_circle, aes(x, y)) +
    coord_fixed(xlim = c(-7.8, 7.8), ylim = c(0, 14)) +
    # coord_cartesian(xlim = c(-7.8, 7.8), ylim = c(0, 14)) +
    ggplot2::theme_void() +
    scico::scale_fill_scico(palette = 'vik',
                            limits = c(-.15, .15),
                            breaks = seq(-.15, .15, .03),
                            labels = c("-15%", "-12%", "-9%", "-6%", "-3%", "0%", "+3%", "+6%", "+9%", "+12%", "+15%"),
                            'FG % vs. Position Average'
    ) +
    guides(fill=guide_legend(
      label.position = 'bottom', 
      title.position = 'top', 
      keywidth=.45,
      keyheight=.15, 
      default.unit="inch", 
      title.hjust = .5,
      title.vjust = -1,
      label.vjust = 3,
      nrow = 1))  +
    theme(text=element_text(size=20,  family="Arial"), # Gill Sans MT), 
          legend.spacing.x = unit(0, 'cm'), 
          legend.title=element_text(size=16), 
          legend.text = element_text(size = rel(0.6)), 
          legend.margin=margin(-10,0,-1,0),
          legend.position = 'bottom',
          legend.box.margin=margin(-30,0,15,0), 
          plot.title = element_text(hjust = 0.5, vjust = -6, size = 23, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 15, vjust = -9), 
          plot.caption = element_text(face = "italic", size = 13), 
          plot.margin = margin(0, -5, 0, -5, "cm"))
}