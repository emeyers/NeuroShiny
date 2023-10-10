
################################################################################
################################# Create Repo ##################################
################################################################################

# Select project
output$git_project_name <- renderUI({
  req(rv$working_dir, rv$projects_available)
  selectInput("git_project_name",
              'Select the project you want to create a new repo for',
              rv$projects_available,
              selected = basename(rv$working_dir))
})

output$github_options <- renderUI({
  req(rv$repo_list, input$git_project_name)
  if(input$git_project_name %in% rv$repo_list){
    list(
      helpText("A repo already exists for this project, you can:"),
      actionButton("push", "Push"),
      actionButton("pull", "Pull"))
  } else {
    list(
      helpText("No repo found, please create one first:"),
      actionButton("create_github", "Create GitHub repo"),
      uiOutput("git_text"))
  }
})

# Update repo options
observeEvent(list(input$git_project_name, input$create_github), {
  git_list <- system2("gh", "repo list", stdout = TRUE, stderr = TRUE)
  rv$repo_list <- sapply(strsplit(git_list, "[/\t]"), function(x) x[2])
})


# Create a new repo if one does not exist with the current project name
observeEvent(input$create_github, {
  # Create new project the new project directory
  create_github_repo(input$git_project_name, rv$working_dir)
  output$git_text <- renderText(paste0("Created new repo called: ",
                                       input$git_project_name))
})

################################################################################
############################## Push/pull buttons ###############################
################################################################################

# Run push button
observeEvent(input$push, {
  req(input$git_project_name)
  gert::git_add()
  gert::git_commit("commit message")
  gert::git_push()
})

# Run pull button
observeEvent(input$pull, {
  req(input$git_project_name)
  gert::git_pull()
})


