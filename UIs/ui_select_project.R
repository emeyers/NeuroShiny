
# Selecting the directory for the whole project
select_proj_tab <- tabPanel(
  title = "Select Project",
  fluidPage(
    fluidRow(
      column(width = 12,
             shinydashboard::box(width = NULL,
                                 status = "danger",
                                 solidHeader = TRUE,
                                 title = "Choose your project directory to get started",
                                 uiOutput('project_option'),
                                 uiOutput('select_project_folder'),
                                 uiOutput('create_project_folder'))
      )
    )
  )
)

