
################################################################################
############################ Saving new raster data ############################
################################################################################

# A list of input boxes that do the following:
output$bin_offer_upload_raster <- renderUI({
  # Input browser for new raster file
  list(fileInput("bin_uploaded_raster",
                 "Upload a zipped file raster data",
                 multiple = TRUE),
       # Input location of
       textInput("bin_uploaded_raster_name",
                 "Where you want the file to be unzipped",
                 rv$raster_base_dir),
       # Action button to save the file
       actionButton("bin_save_raster_to_disk", "Save to disk"),
       # Output the error if neither file nor location given
       uiOutput("bin_save_raster_to_disk_error"))
})

# Elisa - I think you can find a way to no need these error messages
# Output error message if no raster or location was given
output$bin_save_raster_to_disk_error <- renderUI({
  er_bin_save_raster_to_disk_error()
})

# Error message if data or location is missing
er_bin_save_raster_to_disk_error <- eventReactive(input$bin_save_raster_to_disk,{
  validate(
    need(input$bin_uploaded_raster, "Please upload a zipped file raster data"),
    need(input$bin_uploaded_raster_name,
         "Please tell me where you want the file to be unzipped" )
  )
})

# If new raster is saved - Elisa, idk what this does
observeEvent(input$bin_save_raster_to_disk, {
  req(input$bin_uploaded_raster, input$bin_uploaded_raster_name)
  unzip(input$bin_uploaded_raster$datapath, exdir = input$bin_uploaded_raster_name)
})

################################################################################
################################ Create raster #################################
################################################################################

# Elsia idk what this is for too
output$bin_offer_create_raster <- renderUI({
  req(rv$selected_rasters)
  if(rv$raster_bMat){
  temp_matlab_raster_dir_name <- rv$selected_rasters
  # If the directory name ends with _mat, remove _mat
  temp_non_desired_pattern <- '.*_mat$'
  if (grepl(temp_non_desired_pattern, temp_matlab_raster_dir_name) == TRUE){
    temp_r_raster_dir_name <- substr(temp_matlab_raster_dir_name, 1,
                                     nchar(temp_matlab_raster_dir_name) - 4)
    }
    # Then append Rda to the file name
    temp_r_raster_dir_name <- paste0(temp_r_raster_dir_name, "_rda/")

    list(
      helpText(paste0("We can bin raster data in .mat format, but do you want to create raster data in .Rda format? ",
                      "Benefits include the option to plot raster data ")),
      textInput("bin_new_raster",
                "New raster directory name (e.g., data/raster/Zhang_Desimone_7objects_raster_data_rda; by default, we append '_rda' to the matlab raster directory name)",
                temp_r_raster_dir_name),
      numericInput("bin_raster_start_ind",
                   "Index of the sample where the new raster data begin",
                   value = NULL),
      numericInput("bin_raster_end_ind",
                   "Index of the sample where the new raster data end",
                   value = NULL),
      actionButton("bin_create_raster", "Create raster"))
  }
})


# Elisa - or what does this do?
observeEvent(input$bin_create_raster, {
  temp_call <- paste0("NeuroDecodeR:::create_raster_data_from_matlab_raster_data(rv$selected_rasters,",
                     "input$bin_new_raster")
  if(!is.na(input$bin_start_ind)){
    temp_call <- paste0(temp_call, ",input$bin_raster_start_ind")
  }
  if(!is.na(input$bin_end_ind)){
    temp_call <- paste0(temp_call, ",input$bin_raster_end_ind")
  }
  temp_call <- paste0(temp_call,")")
  rv$create_raster_funciton_run <- temp_call
  eval(parse(text = temp_call))
})

# Elisa - and then again what is this
output$bin_show_create_raster_function_run <- renderText(({
  rv$create_raster_funciton_run
}))
