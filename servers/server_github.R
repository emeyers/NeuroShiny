
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



# Create a new repo if one does not exist with the current project name
observeEvent(input$create_github, {
  # Check all the info is in there
  if (input$username == "") {
    output$git_text <- renderText("<br><font color='red'>Please enter a username</font>")
  } else if (input$password == "") {
    output$git_text <- renderText("<br><font color='red'>Please enter a password</font>")
  } else if (input$repo_URL == "") {
    output$git_text <- renderText("<br><font color='red'>Please enter a repo URL first</font>")

  # If a project already exists, throw an error
  } else if (file.exists(file.path("projects", input$git_project_name))) {
    output$git_text <- renderText("<br>A project with this project name already exists")

  # When everything is correct
  } else {
    # Create new project the new project directory
    create_github_repo(input$git_project_name)
    output$git_text <- renderText(paste0("Created new repo called: ",
                                         input$git_project_name))
  }
})

################################################################################
############################## Push/pull buttons ###############################
################################################################################

# Run push button
observeEvent(input$push, {
  req(input$git_project_name)
  gert::git_push(repo = file.path("projects", input$git_project_name))
})

# Run pull button
observeEvent(input$pull, {
  req(input$git_project_name)
  gert::git_pull(repo = file.path("projects", input$git_project_name))
})


