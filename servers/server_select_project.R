

# Select Directory
shinyDirChoose(input, 'projectFolder', roots=c(home='~'), filetypes=c(''))

# show_chosen_project ----
output$show_chosen_project = renderText({
  if(is.integer(input$projectFolder)){
    "No project chosen"
  } else{
    parseDirPath(c(home='~'), input$projectFolder)
  }
})


# Set the current working directory to the project
# This action forces the working directory filter on other browse buttons
observe({
  if (is.list(input$projectFolder)){
    setwd(parseDirPath(c(home='~'), input$projectFolder))
    working_dir <- getwd()
    rv$raster_base_dir <- file.path(working_dir, 'data', 'raster')
    rv$binned_base_dir <- file.path(working_dir, 'data', 'binned')
    rv$result_base_dir <- file.path(working_dir, 'results')
  }
})

