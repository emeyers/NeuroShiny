

# bin_chosen_raster ----
shinyFiles::shinyDirChoose(input, "bin_chosen_raster",
                           roots = c(wd=file.path('data','raster')),
                           filetypes = c("mat", "Rda"))


# bin_show_chosen_raster ----
output$bin_show_chosen_raster = renderText({
  req(rv$raster_cur_dir_name)
  if(is.na(rv$raster_cur_dir_name)){
    "No file chosen yet"
  } else{
    rv$raster_cur_dir_name

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
                                         "'", ,
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

