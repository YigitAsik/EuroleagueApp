tabPanel(
  'Player Stats',
  
  fluidRow(
    column(
      width = 12,
      
      tabsetPanel(
        type = 'tabs',
        
        tabPanel(
          'xPTS',
          
          # div(style = 'display: inline-block; vertical-align:top; width: 210px; 
          #     margin-top: 5px;', 
          #     numericInput(inputId = 'filter_shots', label = 'Min. number of shot attempts', 
          #                  value = 0, min = 0)),
          dataTableOutput('xPTS_data'),
          
          # Nümerik input slider için
          tags$style(type = 'text/css', '.noUi-connect {background: #014cfd;}'),
          # DT sütun renk ayarı
          tags$head(tags$style('#xPTS_data thead th{background-color: #014cfd; color: #fffffa;')),
          # Satır sütun çizgileri
          tags$head(tags$style('#xPTS_data tbody td {border-top: 0.1px solid grey;border-left: 0.1px solid grey;border-right: 0.1px solid grey;}'))
        ),
        
        tabPanel(
          'Hex Plot',
          
          div(id = 'relativeTo',
          radioButtons(inputId = 'against', label = 'Relative to: ', 
                       choices = c('League Avg.', 'Team Avg.', 'Position Avg.'),
                       selected = 'League Avg.', inline = T)),
          div(id = 'four',
          div(id = 'player', 
              selectInput(inputId = 'player_name', label = 'Interested Player', 
                          choices = NULL)),
          div(id = 'onWith',
              selectInput(inputId = 'with', label = 'On With', 
                          choices = NULL, multiple = T)),
          div(id = 'onWithout',
              selectInput(inputId = 'without', label = 'Without', 
                          choices = NULL, multiple = T))
          ),
          div(id = 'buttonPlot',
              actionButton(inputId = 'plot', label = 'Plot', width = 200)),
          
          hr(),
          plotOutput('Hexchart', width = '100%')
          
          
        ),
        
        tabPanel(
          'Possession Filter',
          
          div(id = 'checkBox',
          checkboxGroupInput(inputId = 'poss', label = 'Possessions to Include',
                             choices = c('Low', 'Medium', 'High', 'Very High'),
                             selected = c('Medium', 'High', 'Very High'),
                             inline = T)),
          div(id = 'two',
          selectInput(inputId = 'player_poss', label = 'Player Name',
                      choices = NULL)),
          # selectInput(inputId = 'poss', label = 'Possessions to Filter',
          #             choices = c('None', 'Low', 'Medium', 'High', 'Very High'),
          #             selected = c('Low'),
          #             multiple = F)),
          
          div(id = 'buttonFilter',
              actionButton(inputId = 'filter', label = 'Submit', width=150)),
          div(id = 'explanation',
          p('This allows you to filter stats based on possession importance.'),
          p('Possession importance: How much a particular possession can change the winning probability.')),
          hr(),
          
          dataTableOutput('poss_imp_data'),
          
          tags$head(tags$style('#poss_imp_data thead th{background-color: #014cfd; color: #fffffa;'))
        )
      )
    )
  )
)