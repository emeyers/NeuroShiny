
################################################################################
################################ Select project ################################
################################################################################


# Shiny button to select project folder
shinyDirChoose(input, 'project_folder', roots = c(home=app_base_dir), filetypes = c(''))

# If a project is selected
# Then show project location once selected
output$show_chosen_project <- renderText({
  if(is.integer(input$project_folder)){
    "No project chosen"
  } else {
    parseDirPath(c(home=app_base_dir), input$project_folder)
  }
})

################################################################################
############################### Set directories ################################
################################################################################

# Set the current wd to the selected project directory
# Add filters on additional browse buttons for reactive variables
observe({
  if (is.list(input$project_folder)){
    # Set project as base/working directory
    working_dir <- parseDirPath(c(home=app_base_dir), input$project_folder)
    setwd(working_dir)

    # Set additional directories
    rv$raster_base_dir <- file.path(working_dir, 'data', 'raster')
    rv$binned_base_dir <- file.path(working_dir, 'data', 'binned')
    rv$result_base_dir <- file.path(working_dir, 'results')
  }
})
