# Dependencies -----------------------------------------------------------------
source('Library.R')

# Global -----------------------------------------------------------------------
source('Global.R')

# Front End --------------------------------------------------------------------

ui <- fluidPage(
  
  # CSS
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'shinycss.css')
  ),
  
  # Navbar
  navbarPage(
    title = tagList(
      div(id = 'nameApp', h3('EuroLeague Shiny App')),
      div(id = 'information', p(paste('Most recent update: ', '2025-03-08')))
    ),
    
    # tabPanel('Player'),
    # tabPanel('Team')
  ),
  
  source('UI/data.R', local = TRUE, encoding = 'UTF-8')$value
  
)

# Server -----------------------------------------------------------------------

server <- function(input, output, session){
  
  source(file.path('Server', 'reactiveData.R'),  
         local = TRUE, encoding = 'UTF-8')$value
  source(file.path('Server', 'data.R'),  
         local = TRUE, encoding = 'UTF-8')$value
  
}
# Shiny App ------------------------------------------------------------
shinyApp(ui = ui, server = server)