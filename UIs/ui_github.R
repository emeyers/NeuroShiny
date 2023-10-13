
# Navigating github
github_tab <- tabPanel(
  title = "Connect to GitHub",
  fluidPage(
    fluidRow(
      column(width = 12,
             shinydashboard::box(width = NULL,
                                 status = "danger",
                                 solidHeader = TRUE,
                                 title = "Select or create a new repo",
                                 uiOutput("git_project_name"),
                                 uiOutput("github_options"))
      )
    )
  )
)
