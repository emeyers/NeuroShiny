library(shiny)
source('global.R', local = TRUE)
source('ui.R', local = TRUE)
source('server.R', local = TRUE)



shinyApp(
  ui = myUI,
  server = myserver
)

# adding a projects tab esp to help with changing directories
