##### Expected PTS #####

output$xPTS_data <- renderDataTable({
  
  tmp <- xPTS_data() %>%
    select(
      PlayerNm, TEAM, Position,
      tot_FGA, tot_PTS, tot_xPTS, eFG, xeFG, SM, PTS_Added
    ) %>%
    # filter(tot_FGA >= input$filter_shots) %>%
    arrange(-SM)
  
  colnames(tmp) = c('Player Name', 'Team', 'Position', 'FGA', 'Scored PTS',
                    'Expected PTS', 'eFG%', 'xeFG%', 'PTS Above Expected (Per Shot)', 
                    'PTS Added')
  
  # tmp
  
  
  datatable(
    tmp,
    class = 'display compact order-column', # no wrap
    filter = 'top',
    rownames = F,

    options = list(
      paging = T,
      pageLength = 25,
      autoWidth = T,
      scrollX = T
    )
  )
})

##### HEX #####

observe({
  req(players())
  
  updateSelectInput(session, inputId = 'player_name',
                    choices = players(), selected = 'Shane Larkin')
  updateSelectInput(session, inputId = 'with',
                    choices = players_caps())
  updateSelectInput(session, inputId = 'without',
                    choices = players_caps())
})

observeEvent(input$plot, {
  
  if (input$against == 'League Avg.') {
    
    la <- shots_data() %>%
      # filter(MINUTE <= input$max_min, MINUTE >= input$min_min,
      #        abs(POINTS_A - POINTS_B) <= input$pts_diff) %>%
      group_by(SHOT_ZONE_BASIC, SHOT_ZONE_AREA_2, SHOT_ZONE_DIST) %>%
      summarize(
        fga = n(),
        fgm = sum(FGM_FLAG),
        fg_pct = round(sum(FGM_FLAG) / n(), 3)
      ) %>%
      ungroup() %>% # added recently -- Nov 1
      mutate(
        shot_value = case_when(
          SHOT_ZONE_BASIC %in% c('Above the Break 3', 'Corner 3') ~ 3,
          TRUE ~ 2)
      )
    
    if ((!is.null(input$without)) & (!is.null(input$with))) {
      
      player_shots <- shots_data() %>%
        filter(FULL_NAME == str_to_title(input$player_name)) %>%
        filter_at(vars(Team_A1, Team_A2, Team_A3, Team_A4, Team_A5,
                       Team_B1, Team_B2, Team_B3, Team_B4, Team_B5),
                  any_vars(. %in% input$with)) %>%
        filter_at(vars(Team_A1, Team_A2, Team_A3, Team_A4, Team_A5,
                       Team_B1, Team_B2, Team_B3, Team_B4, Team_B5),
                  all_vars(!. %in% input$without))
      
    } else if (!is.null(input$without)) {
      
      player_shots <- shots_data() %>%
        filter(FULL_NAME == str_to_title(input$player_name)) %>%
        filter_at(vars(Team_A1, Team_A2, Team_A3, Team_A4, Team_A5,
                       Team_B1, Team_B2, Team_B3, Team_B4, Team_B5),
                  all_vars(!. %in% input$without))
      
    } else if (!is.null(input$with)) {
      
      player_shots <- shots_data() %>%
        filter(FULL_NAME == str_to_title(input$player_name)) %>%
        filter_at(vars(Team_A1, Team_A2, Team_A3, Team_A4, Team_A5,
                       Team_B1, Team_B2, Team_B3, Team_B4, Team_B5),
                  any_vars(. %in% input$with))
      
    } else {
      
      player_shots <- shots_data() %>%
        mutate(players_caps = paste0(LastName, ', ', FirstName)) %>%
        filter(FULL_NAME == str_to_title(input$player_name))
      
    }
    
    player_avgs <- player_shots %>%
      group_by(SHOT_ZONE_BASIC, SHOT_ZONE_AREA_2) %>%
      summarize(
        x = mean(coord_x_adj),
        y = mean(coord_y_adj),
        fg_pct = round(100 * (sum(FGM_FLAG) / n()), 1)
      )
    
    hex_data <- calculate_hexbins_from_shots(player_shots, la)
    
    output$Hexchart <- renderPlot({
      plotShothexbinLA(hex_data$hex_data) +
        geom_label(
          data = player_avgs %>%
            filter(
              SHOT_ZONE_BASIC %in% c('Above the Break 3', 'Corner 3') |
                (SHOT_ZONE_BASIC %in% c('Mid-Range')) & (SHOT_ZONE_AREA_2 %in% c('Left Center', 'Center', 'Right Center')) |
                (SHOT_ZONE_BASIC == 'Paint (non-RA)') & (SHOT_ZONE_AREA_2 %in% c('Left', 'Center', 'Right')) |
                (SHOT_ZONE_BASIC == 'Restricted Area') & (SHOT_ZONE_AREA_2 == 'Center')
            ),
          aes(
            x = x,
            y = y,
            label = paste0(as.character(fg_pct), '%')
          ),
          alpha = .75,
          size = 5
        ) +
        labs(title = paste(str_to_title(input$player_name), 'Shot Chart'),
             subtitle = "2024-25 Regular Season") +
        annotate('text', x = 0, y = 0.6, label = '@Hooplytics',
                 size = 8)
    }
    , height = 740
    )
    
  }
  
  else if (input$against == 'Team Avg.'){
    
    if ((!is.null(input$without)) & (!is.null(input$with))) {
      
      player_shots <- shots_data() %>%
        filter(FULL_NAME == str_to_title(input$player_name)) %>%
        filter_at(vars(Team_A1, Team_A2, Team_A3, Team_A4, Team_A5,
                       Team_B1, Team_B2, Team_B3, Team_B4, Team_B5),
                  any_vars(. %in% input$with)) %>%
        filter_at(vars(Team_A1, Team_A2, Team_A3, Team_A4, Team_A5,
                       Team_B1, Team_B2, Team_B3, Team_B4, Team_B5),
                  all_vars(!. %in% input$without))
      
    } else if (!is.null(input$without)) {
      
      player_shots <- shots_data() %>%
        filter(FULL_NAME == str_to_title(input$player_name)) %>%
        filter_at(vars(Team_A1, Team_A2, Team_A3, Team_A4, Team_A5,
                       Team_B1, Team_B2, Team_B3, Team_B4, Team_B5),
                  all_vars(!. %in% input$without))
      
    } else if (!is.null(input$with)) {
      
      player_shots <- shots_data() %>%
        filter(FULL_NAME == str_to_title(input$player_name)) %>%
        filter_at(vars(Team_A1, Team_A2, Team_A3, Team_A4, Team_A5,
                       Team_B1, Team_B2, Team_B3, Team_B4, Team_B5),
                  any_vars(. %in% input$with))
    } else {
      
      player_shots <- shots_data() %>%
        filter(FULL_NAME == str_to_title(input$player_name))
    }
    
    tm <- shots_data() %>%
      select(FULL_NAME, TEAM) %>%
      filter(FULL_NAME == str_to_title(input$player_name)) %>%
      pull(TEAM) %>%
      unique()
    
    ta <- shots_data() %>%
      filter(TEAM == tm) %>%
      group_by(SHOT_ZONE_BASIC, SHOT_ZONE_AREA_2, SHOT_ZONE_DIST) %>%
      summarize(
        fga = n(),
        fgm = sum(FGM_FLAG),
        fg_pct = round(sum(FGM_FLAG) / n(), 3)
      ) %>%
      mutate(
        shot_value = case_when(
          SHOT_ZONE_BASIC %in% c('Above the Break 3', 'Corner 3') ~ 3,
          TRUE ~ 2)
      )
    
    player_avgs <- player_shots %>%
      group_by(SHOT_ZONE_BASIC, SHOT_ZONE_AREA_2) %>%
      summarize(
        x = mean(coord_x_adj),
        y = mean(coord_y_adj),
        fg_pct = round(100 * (sum(FGM_FLAG) / n()), 1)
      )
    
    hex_data <- calculate_hexbins_from_shots(player_shots, ta)
    
    output$Hexchart <- renderPlot({
      plotShothexbinTA(hex_data$hex_data) + 
        geom_label(
          data = player_avgs %>%
            filter(
              SHOT_ZONE_BASIC %in% c('Above the Break 3', 'Corner 3') |
                (SHOT_ZONE_BASIC %in% c('Mid-Range')) & (SHOT_ZONE_AREA_2 %in% c('Left Center', 'Center', 'Right Center')) |
                (SHOT_ZONE_BASIC == 'Paint (non-RA)') & (SHOT_ZONE_AREA_2 %in% c('Left', 'Center', 'Right')) |
                (SHOT_ZONE_BASIC == 'Restricted Area') & (SHOT_ZONE_AREA_2 == 'Center')
            ),
          aes(
            x = x,
            y = y,
            label = paste0(as.character(fg_pct), '%')
          ),
          alpha = .75,
          size = 5
        ) +
        labs(title = paste(str_to_title(input$player_name), 'Shot Chart'),
             subtitle = "2024-25 Regular Season") +
        annotate('text', x = 0, y = 0.6, label = '@Hooplytics',
                 size = 8)
    }
    , height = 740
    )
    
  }
  
  else {
    
    if ((!is.null(input$without)) & (!is.null(input$with))) {
      
      player_shots <- shots_data() %>%
        filter(FULL_NAME == str_to_title(input$player_name)) %>%
        filter_at(vars(Team_A1, Team_A2, Team_A3, Team_A4, Team_A5,
                       Team_B1, Team_B2, Team_B3, Team_B4, Team_B5),
                  any_vars(. %in% input$with)) %>%
        filter_at(vars(Team_A1, Team_A2, Team_A3, Team_A4, Team_A5,
                       Team_B1, Team_B2, Team_B3, Team_B4, Team_B5),
                  all_vars(!. %in% input$without))
      
    } else if (!is.null(input$without)) {
      
      player_shots <- shots_data() %>%
        filter(FULL_NAME == str_to_title(input$player_name)) %>%
        filter_at(vars(Team_A1, Team_A2, Team_A3, Team_A4, Team_A5,
                       Team_B1, Team_B2, Team_B3, Team_B4, Team_B5),
                  all_vars(!. %in% input$without))
      
    } else if (!is.null(input$with)) {
      
      player_shots <- shots_data() %>%
        filter(FULL_NAME == str_to_title(input$player_name)) %>%
        filter_at(vars(Team_A1, Team_A2, Team_A3, Team_A4, Team_A5,
                       Team_B1, Team_B2, Team_B3, Team_B4, Team_B5),
                  any_vars(. %in% input$with))
    } else {
      
      player_shots <- shots_data() %>%
        filter(FULL_NAME == str_to_title(input$player_name))
    }
    
    player_avgs <- player_shots %>%
      group_by(SHOT_ZONE_BASIC, SHOT_ZONE_AREA_2) %>%
      summarize(
        x = mean(coord_x_adj),
        y = mean(coord_y_adj),
        fg_pct = round(100 * (sum(FGM_FLAG) / n()), 1)
      )
    
    position <- shots_data() %>%
      select(FULL_NAME, Position) %>%
      filter(FULL_NAME == str_to_title(input$player_name)) %>%
      pull(Position) %>%
      unique()
    
    pa <- shots_data() %>%
      filter(Position == position) %>%
      group_by(SHOT_ZONE_BASIC, SHOT_ZONE_AREA_2, SHOT_ZONE_DIST) %>%
      summarize(
        fga = n(),
        fgm = sum(FGM_FLAG),
        fg_pct = round(sum(FGM_FLAG) / n(), 3)
      ) %>%
      mutate(
        shot_value = case_when(
          SHOT_ZONE_BASIC %in% c('Above the Break 3', 'Corner 3') ~ 3,
          TRUE ~ 2)
      )
    
    hex_data <- calculate_hexbins_from_shots(player_shots, pa)
    
    output$Hexchart <- renderPlot({
      plotShothexbinPA(hex_data$hex_data) + 
        geom_label(
          data = player_avgs %>%
            filter(
              SHOT_ZONE_BASIC %in% c('Above the Break 3', 'Corner 3') |
                (SHOT_ZONE_BASIC %in% c('Mid-Range')) & (SHOT_ZONE_AREA_2 %in% c('Left Center', 'Center', 'Right Center')) |
                (SHOT_ZONE_BASIC == 'Paint (non-RA)') & (SHOT_ZONE_AREA_2 %in% c('Left', 'Center', 'Right')) |
                (SHOT_ZONE_BASIC == 'Restricted Area') & (SHOT_ZONE_AREA_2 == 'Center')
            ),
          aes(
            x = x,
            y = y,
            label = paste0(as.character(fg_pct), '%')
          ),
          alpha = .75,
          size = 5
        ) +
        labs(title = paste(str_to_title(input$player_name), 'Shot Chart'),
             subtitle = "2024-25 Regular Season") +
        annotate('text', x = 0, y = 0.6, label = '@Hooplytics',
                 size = 8)
    }
    , height = 740
    )
  }
  
})

##### LEVERAGE STATS #####

observe({
  req(leverage_players())
  
  updateSelectInput(session, inputId = 'player_poss',
                    choices = leverage_players(), 
                    selected = 'LARKIN, SHANE - IST')
})

observeEvent(input$filter, {
  
  if (length(input$poss) == 0) {
    
    tmp_df <- leverage_stats_raw_data() %>%
      filter(
        Player_Team == input$player_poss,
      ) %>%
      group_by(Player_Team) %>%
      summarize(
        FG3M = sum(Action == '3FGM'),
        FG3A = sum(Action == '3FGA'),
        FG2M = sum(Action == '2FGM'),
        FG2A = sum(Action == '2FGA'),
        FTM = sum(Action == 'FTM'),
        FTA = sum(Action == 'FTA')
      ) %>%
      ungroup() %>%
      mutate(
        FG3A = FG3A + FG3M,
        FG2A = FG2A + FG2M,
        FTA = FTA + FTM
      ) %>%
      mutate(
        FTM_PCT = round(FTM / FTA, 3),
        FG2_PCT = round(FG2M / FG2A, 3),
        FG3_PCT = round(FG3M / FG3A, 3)
      ) %>%
      separate(Player_Team, into = c('Player', 'Team'), sep = ' - ')
    
    output$poss_imp_data <- renderDataTable({
      
      datatable(
        tmp_df,
        class = 'display compact order-column', # no wrap
        filter = 'none',
        rownames = F,
        
        options = list(
          paging = T,
          autoWidth = F,
          scrollX = T
        ))
      
    })
    
  } else if (length(input$poss) < 4) {
    
    tmp_df <- leverage_stats_raw_data() %>%
      filter(
        (Player_Team == input$player_poss) &
        (poss_val_cat %in% input$poss)
      ) %>%
      group_by(Player_Team) %>%
      summarize(
        FG3M = sum(Action == '3FGM'),
        FG3A = sum(Action == '3FGA'),
        FG2M = sum(Action == '2FGM'),
        FG2A = sum(Action == '2FGA'),
        FTM = sum(Action == 'FTM'),
        FTA = sum(Action == 'FTA')
      ) %>%
      ungroup() %>%
      mutate(
        FG3A = FG3A + FG3M,
        FG2A = FG2A + FG2M,
        FTA = FTA + FTM
      ) %>%
      mutate(
        FTM_PCT = round(FTM / FTA, 3),
        FG2_PCT = round(FG2M / FG2A, 3),
        FG3_PCT = round(FG3M / FG3A, 3)
      ) %>%
      separate(Player_Team, into = c('Player', 'Team'), sep = ' - ')
    
    output$poss_imp_data <- renderDataTable({
      
      datatable(
        tmp_df,
        class = 'display compact order-column', # no wrap
        filter = 'none',
        rownames = F,
        
        options = list(
          paging = T,
          autoWidth = F,
          scrollX = T
        ))
    
    })
  }
  
  else {
    
    tmp_df <- leverage_stats_raw_data() %>%
      filter(
        Player_Team == input$player_poss
      ) %>%
      group_by(Player_Team, poss_val_cat) %>%
      summarize(
        POSS = n(),
        FG3M = sum(Action == '3FGM'),
        FG3A = sum(Action == '3FGA'),
        FG2M = sum(Action == '2FGM'),
        FG2A = sum(Action == '2FGA'),
        FTM = sum(Action == 'FTM'),
        FTA = sum(Action == 'FTA')
      ) %>%
      ungroup() %>%
      mutate(
        FG3A = FG3A + FG3M,
        FG2A = FG2A + FG2M,
        FTA = FTA + FTM
      ) %>%
      mutate(
        FTM_PCT = round(FTM / FTA, 3),
        FG2_PCT = round(FG2M / FG2A, 3),
        FG3_PCT = round(FG3M / FG3A, 3)
      ) %>%
      separate(Player_Team, into = c('Player', 'Team'), sep = ' - ') %>%
      rename('Possession Importance' = poss_val_cat) %>%
      arrange(`Possession Importance`)
    
    
    output$poss_imp_data <- renderDataTable({
      
      datatable(
        tmp_df,
        class = 'display compact order-column', # no wrap
        filter = 'none',
        rownames = F,
        
        options = list(
          paging = T,
          autoWidth = F,
          scrollX = T
        ))
      
    })
    
  }
  
})
