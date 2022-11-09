

select_proj_tab <-tabPanel(
  title = "Select Project",
  fluidPage(
    fluidRow(
      column(width = 12,
             shinydashboard::box(width = NULL,
                                 status = "danger",
                                 solidHeader = TRUE,
                                 title = "Choose your project directory to get started",
                                 shinyDirButton('projectFolder', 'Browse',
                                                'Please select a folder'),
                                 helpText("Current Project: "),
                                 textOutput("show_chosen_project")
             )
      )
    )
  )
)
  shinyUI(
    bootstrapPage(

      )
    )