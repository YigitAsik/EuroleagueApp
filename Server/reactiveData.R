# For PTS Above Expected
xPTS_data <- reactive({
  read.csv('Data/xPTS_20250308.csv', stringsAsFactors = FALSE) %>% 
    select(
      PlayerNm, LastNm, FirstNm, TEAM, Position,
      tot_xPTS, tot_PTS, tot_FGA, eFG, xeFG, SM, PTS_Added
    )
})

# For Shot Chart
shots_data <- reactive({
  read.csv('Data/EL_Shots_20250308_processed.csv') %>%
    mutate(players_caps = paste0(LastName, ', ', FirstName))
})

players <- reactive({
  read.csv('Data/EL_Shots_20250308_processed.csv') %>%
    mutate(players_caps = paste0(LastName, ', ', FirstName)) %>%
    pull(FULL_NAME) %>%
    unique() %>% sort()
})

players_caps <- reactive({
  read.csv('Data/EL_Shots_20250308_processed.csv') %>%
    mutate(players_caps = paste0(LastName, ', ', FirstName)) %>%
    pull(players_caps) %>%
    unique() %>% sort()
})

# For Leverage Stats
leverage_stats_raw_data <- reactive({
  read.csv('Data/df_poss_data_stats_20250308.txt') %>%
    mutate(poss_val_cat = factor(poss_val_cat,
                                 c('Low', 'Medium', 'High', 'Very High')))
})

leverage_players <- reactive({
  read.csv('Data/df_poss_data_stats_20250308.txt') %>%
    pull(Player_Team) %>%
    unique() %>% sort()
})