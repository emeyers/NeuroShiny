
################################################################################
################################# Create Repo ##################################
################################################################################

# Display raster name
output$git_project_name <- renderText({
  req(rv$working_dir)
  if(is.na(rv$working_dir)){
    "No projects available <br>"
  } else{
    paste0("<br><font size='+1' color='black'><strong> Current Project: ",
           "</strong></font><font size='+1'>", basename(rv$working_dir), "</font><br>")

  }
})


output$github_options <- renderUI({
  req(rv$repo_list, rv$working_dir)
  if(basename(rv$working_dir) %in% rv$repo_list){
    list(
      helpText("A repo already exists for this project"),
      textInput("commit_message",
                "To push, add a commit message (required)",
                "New Commit"),
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
observeEvent(list(rv$working_dir, input$create_github), {
  git_list <- system2("gh", "repo list", stdout = TRUE, stderr = TRUE)
  rv$repo_list <- sapply(strsplit(git_list, "[/\t]"), function(x) x[2])
})


# Create a new repo if one does not exist with the current project name
observeEvent(input$create_github, {
  # Create new project the new project directory
  create_github_repo(basename(rv$working_dir), rv$working_dir)
  output$git_text <- renderText(paste0("Created new repo called: ",
                                       basename(rv$working_dir)))
})

################################################################################
############################## Push/pull buttons ###############################
################################################################################

# Run push button
observeEvent(input$push, {
  req(rv$working_dir, input$commit_message)
  status_output <- system2("git", "status", stdout = TRUE)

  # Check if there are changes to commit
  if ("nothing to commit, working tree clean" %!in% status_output) {
    system2("git", "add .", stdout = TRUE)
    gert::git_commit(as.character(input$commit_message))
    gert::git_push()
  }
})

# Run pull button
observeEvent(input$pull, {
  req(rv$working_dir)
  gert::git_pull()
})


