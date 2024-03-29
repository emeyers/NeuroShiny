
################################################################################
############################ Load and prep the data ############################
################################################################################

# Create list of valid Rdas from results
observeEvent(list(rv$working_dir, input$DC_run_script),{
  result_folder <- file.path(rv$decoding_result_files_base_dir)
  result_list <- list.files(result_folder)
  rv$valid_result_list <- c()

  # Append only files with values from the prefix list
  for(result in result_list){
    df <- get(load(file.path(result_folder, result)))
    valid_result <- all(sapply(single_result_prefix, function(p) any(startsWith(names(df), p))))
    if(valid_result){
      rv$valid_result_list <- append(rv$valid_result_list, result)
    }
  }
})

# Select single result from available .Rda results
output$plot_chosen_result <- renderUI({
  selectInput("plot_chosen_result", "", rv$valid_result_list)
})

# If a file has been chosen
# Then load the data and save the data paths and results
observeEvent(list(rv$decoding_result_files_base_dir, input$plot_chosen_result),{
  req(input$plot_chosen_result)
  # If statement to reset plots when there is a new directory
  if (input$plot_chosen_result %in% list.files(rv$decoding_result_files_base_dir)){
    # Save file path as reactive variable
    rv$result_chosen <- file.path(rv$decoding_result_files_base_dir, input$plot_chosen_result)
    # Load and save results as reactive variable
    load(rv$result_chosen)
    rv$result_data <- DECODING_RESULTS

  } else {
    rv$result_data <- NULL
  }

})

# Show the current file that has been chose to plot
output$plot_show_chosen_result <- renderText({
  if(is.null(rv$result_chosen)){
    "No file chosen yet"
  } else {
    base_char <- gsub(app_base_dir, "", rv$decoding_result_files_base_dir)
    file.path(base_char, input$plot_chosen_result)
  }
})

################################################################################
########################## Plotting the single results #########################
################################################################################

# Plot time series using NeuroDecodeR function
output$plot_timeseries <- renderPlot({
  validate(
    need(rv$result_data, "No data uploaded")
  )
  validate(
    need(rv$result_data$rm_main_results, "This data does not have main results")
  )
  plot(rv$result_data$rm_main_results, type = "line",
       results_to_show = input$plot_timeseries_result_type)
})

# Plot TCD using NeuroDecodeR function
output$plot_tcd <- renderPlot({
  validate(
    need(rv$result_data, "No data uploaded")
  )
  validate(
    need(rv$result_data$rm_main_results, "This data does not have main results")
  )
  req(rv$result_data)
  plot(rv$result_data$rm_main_results,
       results_to_show = input$plot_tcd_result_type)
})

# Plot CM using NeuroDecodeR function
output$plot_cm <- renderPlot({
  validate(
    need(rv$result_data, "No data uploaded")
  )
  validate(
    need(rv$result_data$rm_main_results, "This data does not have a confusion matrix")
  )
  plot(rv$result_data$rm_confusion_matrix,
       results_to_show = input$plot_cm_result_type)
})


################################################################################
################################## PDF Output ##################################
################################################################################

# Create list of valid PDFs from the results
observeEvent(list(rv$working_dir, input$plot_chosen_result, input$DC_run_script), {
  req(rv$working_dir, input$plot_chosen_result)

  # Resetting this value, only changed if a pdf is found later
  rv$found_single_result_pdf <- FALSE

  # Getting the analysis ID
  analysis_id <- sub("\\.[Rr]da$", "", input$plot_chosen_result)

  # Find the directories that end in PDF
  result_dir <- list.dirs(rv$decoding_results_base_dir,
                          full.names = FALSE, recursive = FALSE)
  pdf_dirs <- result_dir[grep("_pdf$", result_dir)]
  pdf_list <- c()

  # Find all the pdfs in pdf directories
  for(directory in pdf_dirs) {
    pdfs <- list.files(file.path(rv$decoding_results_base_dir, directory),
                       pattern = ".pdf$")
    for (p in pdfs){
      curr_pdf <- file.path(rv$decoding_results_base_dir, directory, p)
      text <- pdf_text(curr_pdf)

      if(any(grepl(paste("The analysis ID is:", analysis_id), text))){
        dest_pdf <- file.path(app_base_dir, "www", "single_result_to_show.pdf")
        file.copy(curr_pdf, dest_pdf)
        rv$found_single_result_pdf <- TRUE
      }
    }
  }
})



# Show the pdf
output$show_single_result_pdf <- renderUI({
  validate(
    need(input$plot_chosen_result, "No pdfs available")
  )
  validate(
    need(rv$found_single_result_pdf, "No available pdf for this analysis id")
  )
  tags$iframe(style="height:600px; width:100%",
              src = "single_result_to_show.pdf")
})
