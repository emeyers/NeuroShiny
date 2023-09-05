
# Navigating github
github_tab <- tabPanel(
  title = "Connect to GitHub",
  fluidPage(
    fluidRow(
      column(width = 12,
             shinydashboard::box(width = NULL,
                                 status = "danger",
                                 solidHeader = TRUE,
                                 title = "Create a new repo",
                                 textInput("username", "Enter GitHub username"),
                                 textInput("password", "Enter GitHub password"),
                                 helpText(" "),
                                 uiOutput("git_project_name"),
                                 textInput("repo_URL", "Enter GitHub repo URL"),
                                 actionButton("create_github", "Create GitHub repo"),
                                 actionButton("push", "Push"),
                                 actionButton("pull", "Pull"),
                                 uiOutput("git_text"))
      )
    )
  )
)
