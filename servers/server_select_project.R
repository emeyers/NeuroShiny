
################################################################################
################################ Select project ################################
################################################################################


# Shiny button to select project folder
shinyDirChoose(input, 'project_folder', roots = c(home=app_base_dir), filetypes = c(''))

# Show project location once selected
# Set directory and show warnings
# Set prefix of directory
output$show_chosen_project <- renderText({
  if(is.integer(input$project_folder)) {
    "No project chosen"
  } else {
    rv$working_dir <- parseDirPath(c(home=app_base_dir), input$project_folder)
    setwd(rv$working_dir)
    message_output <- rv$working_dir

    # Error Messages for file types
    if(!dir.exists('data')) {
      data_warn <- "<font color='red'>Warning: Your project has no data folder</font>"
      message_output <- paste(message_output, data_warn, sep = "<br>")
    } else {
      if (!dir.exists('data/raster')) {
        raster_warn <- "<font color='red'>Warning: Your data folder has no raster folder</font>"
        message_output <- paste(message_output, raster_warn, sep = "<br>")
      } else if (!dir.exists('data/binned')) {
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
############################### Set directories ################################
################################################################################

# Set the current wd to the selected project directory
# Add filters on additional browse buttons for reactive variables
observeEvent(rv$working_dir,{
  if (is.list(input$project_folder)){
    # Set additional directories
    rv$raster_base_dir <- file.path('data', 'raster')
    rv$binned_base_dir <- file.path('data', 'binned')
    rv$result_base_dir <- file.path('results')
    rv$decoding_results_base_dir <- file.path('results', 'decoding_results')
  }

})
