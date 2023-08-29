
################################################################################
################################ Upload raster #################################
################################################################################

# Selecting raster file to bin
# Also updates if new rasters are added
observeEvent(list(rv$binned_base_dir, input$bin_save_raster_to_disk),{
  req(rv$binned_base_dir)
  rv$folders_to_bin <- list.dirs(rv$raster_base_dir, full.names = FALSE,
                                  recursive = FALSE)
})

output$bin_chosen_raster <- renderUI({
  selectInput("bin_chosen_raster", "", rv$folders_to_bin)
})

# Setting the file name and list of rasters
observe({
  req(input$bin_chosen_raster)
  # Set current directory name to the base directory
  rv$selected_rasters <- file.path(rv$working_dir, rv$raster_base_dir,
                                   input$bin_chosen_raster)
  rv$raster_list <- list.files(rv$selected_rasters,
                               pattern = "\\.[rR]da$|\\.csv$|\\.mat$")
  # Saving the number of files
  rv$raster_num_neuron <- length(rv$raster_list)
})


# Display raster name
output$bin_show_chosen_raster <- renderText({
  req(rv$selected_rasters)
  if(is.na(rv$selected_rasters)){
    "No file chosen yet"
  } else{
    file.path(basename(rv$working_dir), rv$result_base_dir,
              input$bin_chosen_raster)
  }
})


################################################################################
############################### Binning the data ###############################
################################################################################

# Create default prefix for binning
reactive_bin_prefix <- reactive({
  req(rv$working_dir)
  # First three letters of project name
  toupper(substr(basename(rv$working_dir), 1, 3))
})

# Update the bin prefix default value in the ui
observe({
  updateTextInput(session, "bin_file_name_prefix", value = reactive_bin_prefix())
})


# Run binning if button is pressed and creating display text
observeEvent(input$bin_the_data,{
  req(input$bin_bin_width, input$bin_step_size,
      rv$selected_rasters, input$bin_file_name_prefix)

  # Location of new binned file
  binned_name <- paste0(rv$binned_base_dir, "/", input$bin_file_name_prefix)

  # To call the function
  decoder_call <- paste0("NeuroDecodeR:::create_binned_data(rv$selected_rasters, ",
                         "binned_name, input$bin_bin_width, input$bin_step_size")

  # To show the function in the ui
  rv$create_bin_function <- list(paste0("'", rv$selected_rasters,"'"),
                                 paste0("'", binned_name, "'"),
                                 input$bin_bin_width, input$bin_step_size)

  # Add optional values if entered
  if(!is.na(input$bin_start_time)){
    decoder_call <- paste0(decoder_call, ", input$bin_start_time")
    rv$create_bin_function <- paste0(rv$create_bin_function, input$bin_start_time)
  }
  if(!is.na(input$bin_end_time)){
    decoder_call <- paste0(decoder_call, ", input$bin_end_time")
    rv$create_bin_function <- paste0(rv$create_bin_function, input$bin_end_time)
  }

  # Complete the functions
  decoder_call <- paste0(decoder_call,")")
  rv$create_bin_function  <- paste0("NeuroDecodeR:::create_binned_data(",
                   paste(rv$create_bin_function, collapse = ", "), ")")

  # Evaluate the code
  eval(parse(text = decoder_call))

})

# Display bin function run
output$show_binning_function <- renderText({
  rv$create_bin_function
})

