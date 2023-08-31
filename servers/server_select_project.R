
# Select a project or create a new project
output$project_option <- renderUI({
  selectInput("project_option", '', c("", "Select a Project", "Create a Project"))
})

################################################################################
################################ Select project ################################
################################################################################

# Shiny button to select project folder
output$select_project_folder <- renderUI({
  req(input$project_option)
  if(input$project_option == "Select a Project"){
    projects_available <- list.dirs(file.path(app_base_dir, "projects"),
                                    full.names = FALSE, recursive = FALSE)

    list(selectInput("select_project_folder", 'Please select a folder', projects_available),
         helpText("Current Project: "),
         htmlOutput("show_chosen_project"))
  }
})

# Show project location once selected
# Set directory and show warnings
# Set prefix of directory
output$show_chosen_project <- renderText({
  req(input$select_project_folder)
  if(is.integer(input$select_project_folder)) {
    "No project chosen"
  } else {
    rv$working_dir <- file.path(app_base_dir, "projects", input$select_project_folder)
    setwd(rv$working_dir)

    # Error Messages for file types
    message_output <- rv$working_dir
    if(!dir.exists('data')) {
      data_warn <- "<font color='red'>Warning: Your project has no data folder</font>"
      message_output <- paste(message_output, data_warn, sep = "<br>")
    } else {
      if (!dir.exists('data/raster_data')) {
        raster_warn <- "<font color='red'>Warning: Your data folder has no raster folder</font>"
        message_output <- paste(message_output, raster_warn, sep = "<br>")
      } else if (!dir.exists('data/binned_data')) {
        binned_warn <- "<font color='red'>Warning: Your data folder has no binned folder</font>"
        message_output <- paste(message_output, binned_warn, sep = "<br>")
      }
    }

    if(!dir.exists('results')) {
      results_warn <- "<font color='red'>Warning: Your project has no results folder</font>"
      message_output <- paste(message_output, results_warn, sep = "<br>")
    }

    message_output
  }
})

################################################################################
################################ Create project ################################
################################################################################

output$create_project_folder <- renderUI({
  req(input$project_option)
  if(input$project_option == "Create a Project"){
    list(
      textInput("new_project_name",
                "What do you want to call your new project?"),
      actionButton("create_project", "Create your project and set directory"),
      helpText("Current Project: "),
      htmlOutput("show_created_project"))
  }
})

observeEvent(input$create_project,{
  req(input$new_project_name)
  rv$working_dir <- file.path(app_base_dir, "projects", input$new_project_name)
  if(dir.exists(rv$working_dir)) {
    output$show_created_project <- renderText("<font color='red'> This project already
                                      exists, please select another name </font>")
  } else {
    create_analysis_project(input$new_project_name, file.path(app_base_dir, "projects"))
    setwd(rv$working_dir)
    output$show_created_project <- renderText(file.path("projects",
                                                        input$new_project_name))
  }
})


################################################################################
############################### Set directories ################################
################################################################################

# Set the current wd to the selected project directory
# Add filters on additional browse buttons for reactive variables
observeEvent(rv$working_dir,{
  if (!is.null(rv$working_dir)){
    # Set additional directories
    rv$raster_base_dir <- file.path(rv$working_dir, 'data', 'raster_data')
    rv$binned_base_dir <- file.path(rv$working_dir, 'data', 'binned_data')
    rv$result_base_dir <- file.path(rv$working_dir, 'results')
    rv$decoding_results_base_dir <- file.path(rv$working_dir, 'results',
                                    'decoding_results')
    rv$decoding_result_files_base_dir <- file.path(rv$working_dir, 'results',
                                              'decoding_results',
                                              'decoding_result_files')
  }

})
