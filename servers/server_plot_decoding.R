

# plot_chosen_result ----
shinyFiles::shinyFileChoose(input, "plot_chosen_result",
                            root = c(wd=result_base_dir),
                            filetypes = "Rda")

observe({
  req(input$plot_chosen_result)
  temp_df_file <- shinyFiles::parseFilePaths(c(wd= rv$result_base_dir),
                                             input$plot_chosen_result)
  req(temp_df_file$datapath)
  rv$result_chosen <- temp_df_file$datapath
  load(rv$result_chosen)
  rv$result_data <- DECODING_RESULTS
})


# plot_show_chosen_result ----
output$plot_show_chosen_result = renderText({
  if(is.na(rv$result_chosen)){
    "No file chosen yet"
  } else{
    basename(rv$result_chosen)
  }
})


# plot_timeseries ----
output$plot_timeseries = renderPlot({
  req(rv$result_data)
  plot(rv$result_data$rm_main_results, type = "line",
       results_to_show = input$plot_timeseries_result_type)
})


# plot_tcd ----
output$plot_tcd = renderPlot({
  req(rv$result_data)
  plot(rv$result_data$rm_main_results,
       results_to_show = input$plot_tcd_result_type)
})

# plot_cm ----
output$plot_cm = renderPlot({
  req(rv$result_data)
  plot(rv$result_data$rm_confusion_matrix,
       results_to_show = input$plot_cm_result_type)
})

observeEvent(input$plot_create_pdf,{
  req(rv$result_chosen, input$plot_timeseries_result_type)
  append_result_to_pdf_and_knit(rv$result_chosen, input$plot_timeseries_result_type)
  print("done")
  output$Plot_pdf <- renderUI({
    req(rv$result_chosen)
    pdf_name <- gsub("Rmd", "pdf", rv$save_script_name)
    tags$iframe(style="height:600px; width:100%", src = pdf_name)
  })
})

# plot_pdf ----
# Elisa Not sure if this is right, used to be DC_pdf
output$plot_pdf <- renderUI({
  if (is.null(rv$save_script_name)){
    "The results will appear as a pdf below once the code is done running."
  }else{
    pdf_name <- gsub("Rmd", "pdf", basename(rv$save_script_name))
    tags$iframe(style="height:600px; width:100%", src = pdf_name)
  }
})

