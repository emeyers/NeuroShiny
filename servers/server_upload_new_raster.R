
################################################################################
############################ Saving new raster data ############################
################################################################################

# A text box with the unzipped folder's new location
output$bin_uploaded_raster_location <- renderUI({
  base_char <- gsub(app_base_dir, "", rv$raster_base_dir)
  # Input browser for new raster file
  textInput("bin_uploaded_raster_location",
            "Where do you want the file to be unzipped in your current project",
            base_char)
})

# Find the name of the current data and save the name of the full path
# This is to check if it already exists in the next observeEvent
observeEvent(input$bin_uploaded_raster, {
  new_data_folder <- sub("\\.zip$", "", input$bin_uploaded_raster$name)
  rv$bin_full_raster_path <- file.path(input$bin_uploaded_raster_location, new_data_folder)
})

# If button to save is pressed, then unzip the data
observeEvent(input$bin_save_raster_to_disk, {
  req(rv$bin_full_raster_path)
  # Error messages
  if(dir.exists(rv$bin_full_raster_path)) {
    output$upload_error <- renderText("<font color='red'> This data already
                                      exists, please select another file or
                                      change the name of your zip </font>")
  } else if(tolower(file_ext(input$bin_uploaded_raster$name)) != "zip") {
    output$upload_error <- renderText("<font color='red'> This is not a .zip
                                      file</font>")
  } else {
    output$upload_error <- renderText(NULL)
    unzip(input$bin_uploaded_raster$datapath, exdir = input$bin_uploaded_raster_location)
  }
})

