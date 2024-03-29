
# Root file of NeuroShiny

library(shiny)
source('global.R', local = TRUE)
source('ui.R', local = TRUE)
source('server.R', local = TRUE)

shinyApp(
  ui = myUI,
  server = myServer, enableBookmarking = "url"
)
