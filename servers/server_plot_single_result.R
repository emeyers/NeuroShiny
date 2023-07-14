
################################################################################
############################ Load and prep the data ############################
################################################################################

# Select .Rda file from computed results file
shinyFiles::shinyFileChoose(input, "plot_chosen_result",
                            root = c(wd=file.path('.', 'results')),
                            filetypes = "Rda")

# If a file has been chosen
# Then load the data and save the data paths and results
observe({
  # Save file path as reactive variable
  req(input$plot_chosen_result)
  temp_df_file <- shinyFiles::parseFilePaths(c(wd=rv$result_base_dir),
                                             input$plot_chosen_result)
  req(temp_df_file$datapath)
  rv$result_chosen <- temp_df_file$datapath

  # Load and save results as reactive variable
  load(rv$result_chosen)
  rv$result_data <- DECODING_RESULTS
})

# Show the current file that has been chose to plot
output$plot_show_chosen_result <- renderText({
  if(is.na(rv$result_chosen)){
    "No file chosen yet"
  } else {
    basename(rv$result_chosen)
  }
})

################################################################################
########################## Plotting the single results #########################
################################################################################

# Plot time series using NeuroDecodeR function
output$plot_timeseries <- renderPlot({
  req(rv$result_data)
  plot(rv$result_data$rm_main_results, type = "line",
       results_to_show = input$plot_timeseries_result_type)
})

# Plot TCD using NeuroDecodeR function
output$plot_tcd <- renderPlot({
  req(rv$result_data)
  plot(rv$result_data$rm_main_results,
       results_to_show = input$plot_tcd_result_type)
})

# Plot CM using NeuroDecodeR function
output$plot_cm <- renderPlot({
  req(rv$result_data)
  plot(rv$result_data$rm_confusion_matrix,
       results_to_show = input$plot_cm_result_type)
})

################################################################################
################################## PDF Output ##################################
################################################################################

# If the create button has been pressed, then render the pdf
# elisa idk if this is how you wanna do this
observeEvent(input$plot_create_pdf, {
  req(rv$result_chosen, input$plot_timeseries_result_type)
  # This function doesn't exist
  append_result_to_pdf_and_knit(rv$result_chosen, input$plot_timeseries_result_type)
  print("done")
  output$plot_pdf <- renderUI({
    req(rv$result_chosen)
    pdf_name <- gsub("Rmd", "pdf", rv$save_script_name)
    tags$iframe(style = "height:600px; width:100%", src = pdf_name)
  })
})

# This exists above so i removed it but it still doesn't work
# Elisa Not sure if this is right, used to be DC_pdf also it doesn't work
#output$plot_pdf <- renderUI({
#  if (is.null(rv$save_script_name)){
#    "The results will appear as a pdf below once the code is done running."
#  } else {
#    pdf_name <- gsub("Rmd", "pdf", basename(rv$save_script_name))
#    tags$iframe(style="height:600px; width:100%", src = pdf_name)
#  }
#})


