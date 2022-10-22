

# bin_chosen_raster ----
shinyFiles::shinyDirChoose(input, "bin_chosen_raster",
                           roots = c(wd=raster_base_dir),
                           filetypes = c("mat", "Rda"))

observe({
  req(input$bin_chosen_raster)
  rv$raster_cur_dir_name <- shinyFiles::parseDirPath(c(wd= rv$raster_base_dir),
                                                     input$bin_chosen_raster)

  req(rv$raster_cur_dir_name)
  mat_files_in_raster_dir <- list.files(rv$raster_cur_dir_name,
                                        pattern = "\\.mat$")

  if(length(mat_files_in_raster_dir) > 0){
    rv$raster_bMat <- TRUE
  }else{
    rv$raster_bMat <-FALSE
    rda_files_in_raster_dir <- list.files(rv$raster_cur_dir_name,
                                          pattern = "\\.[rR]da$")
    rv$raster_num_neuron <- length(rda_files_in_raster_dir)

    if(rv$raster_num_neuron > 0){
      rv$raster_bRda <- TRUE
      rv$raster_cur_file_name <- rda_files_in_raster_dir[rv$raster_cur_neuron]
      load(file.path(rv$raster_cur_dir_name, rv$raster_cur_file_name))
      temp_dfRaster <- select(raster_data, starts_with("time."))
      temp_mRaster <- as.matrix(temp_dfRaster)
      rownames(temp_mRaster) <- 1:dim(temp_mRaster)[1]
      colnames(temp_mRaster) <- gsub("time.", "", colnames(temp_mRaster))
      rv$mRaster_cur_data <- temp_mRaster
    }else{
      rv$raster_bRda <- FALSE
      # this doesn't work; observe is for action not calculation
      # validate("Only accept raster data in .mat or .Rda format !")
    }
  }
})

# bin_show_chosen_raster ----
output$bin_show_chosen_raster = renderText({
  req(rv$raster_cur_dir_name)
  if(is.na(rv$raster_cur_dir_name)){
    "No file chosen yet"
  } else{
    basename(rv$raster_cur_dir_name)

  }
})

# bin_the_data ----
observeEvent(input$bin_the_data,{
  if(rv$raster_bRda){

    # data binned data in the director data/binned
    #binned_basename <- trimws(file.path("data", "binned", " "))

    temp_call <- paste0("NeuroDecodeR:::create_binned_data(rv$raster_cur_dir_name, ",
                       "paste0(binned_base_dir, input$bin_prefix_of_binned_file_name),",
                       "input$bin_bin_width, input$bin_step_size")

    if(!is.na(input$bin_start_ind)){
      temp_call <- paste0(temp_call, ",input$bin_start_ind")
    }
    if(!is.na(input$bin_end_ind)){
      temp_call <- paste0(temp_call, ",input$bin_end_ind")
    }
    temp_call <- paste0(temp_call,")")
    #rv$create_bin_function_run <- temp_call

    rv$create_bin_function_run <- paste0("NeuroDecodeR:::create_binned_data('",
                                         rv$raster_cur_dir_name, "', ",
                                         #"'", binned_basename,
                                         "'", binned_base_dir,
                                         input$bin_prefix_of_binned_file_name, "', ",
                                         input$bin_bin_width, ", ", input$bin_step_size, ")")

    eval(parse(text = temp_call))

  } else if(rv$raster_bMat){

    temp_call <- paste0("NeuroDecodeR:::create_binned_data_from_matlab_raster_data(rv$raster_cur_dir_name,",
                       "input$bin_prefix_of_binned_file_name,",
                       "input$bin_bin_width, input$bin_step_size")
    if(!is.na(input$bin_start_ind)){
      temp_call <- paste0(temp_call, ",input$bin_start_ind")
    }
    if(!is.na(input$bin_end_ind)){
      temp_call <- paste0(temp_call, ",input$bin_end_ind")
    }
    temp_call <- paste0(temp_call,")")
    rv$create_bin_function_run <- temp_call
    eval(parse(text = temp_call))

  }

})

# bin_show_create_bin_function_run ----
output$bin_show_create_bin_function_run = renderText({
  rv$create_bin_function_run
})

