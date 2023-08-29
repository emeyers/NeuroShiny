
################################################################################
######################### Initialize reactive variables ########################
################################################################################

rv$editable_cols <- NULL
rv$result_names_legend_names <- NULL
rv$analysis_ID_legend_names <- NULL

################################################################################
############################## Load manifest data ##############################
################################################################################

# Create list of valid manifests from results
observeEvent(rv$binned_base_dir,{
  prefix <- list("analysis_ID", "result_name", "ds_", "cv_", "cl_", "fp_", "rm_")
  result_folder <- rv$decoding_results_base_dir
  result_list <- list.files(result_folder)
  rv$valid_manifest_list <- c()

  # Append only files with values from the prefix list
  for(result in result_list){
    df <- get(load(file.path(result_folder, result)))
    valid_result <- all(sapply(prefix, function(p) any(startsWith(names(df), p))))
    if(valid_result){
      rv$valid_manifest_list <- append(rv$valid_manifest_list, result)
    }
  }
})

# Select the manifest data
output$manifest_data <- renderUI({
  selectInput("manifest_data", "", rv$valid_manifest_list)
})

# Load the selected data and save variables
observeEvent(input$manifest_data, {
  req(input$manifest_data)
  # Find and load the data of the manifest file
  rv$manifest_chosen <- file.path(rv$decoding_results_base_dir, input$manifest_data)
  # Load and save results as reactive variable
  load(rv$manifest_chosen)
  rv$manifest_data <- manifest_df

  # Columns to disable other than result_name
  # Hard coded only for result_name
  result_ind <- which(colnames(rv$manifest_data) == "result_name")
  all_cols <- 1:length(rv$manifest_data)
  rv$editable_cols <- all_cols[-c(result_ind)]

  # Saving the values of columns
  rv$result_names_legend_names <- rv$manifest_data$result_name
  rv$analysis_ID_legend_names <- rv$manifest_data$analysis_ID
})

# Show the manifest data file path
output$show_chosen_manifest <- renderText({
  if(is.null(input$manifest_data)){
    "No manifest file chosen"
  } else {
    if (input$manifest_data < 1){
      "No manifest file available"
    } else {
      base_char <- gsub(app_base_dir, "", rv$decoding_results_base_dir)
      file.path(base_char, input$manifest_data)
    }

  }
})

################################################################################
############################# Manifest data table ##############################
################################################################################

# Show the manifest data in table format
output$manifest_table <- renderDT({
  req(rv$manifest_data)
  # Disable non-editable columns
  DT::datatable(rv$manifest_data,
                editable = list(target = "cell",
                                disable = list(columns = rv$editable_cols)),
                options = list(scrollX = TRUE))
})

# When the data changes, or the legend label selection changes:
# Assign the current editable column and legend names accordingly
observeEvent(list(input$manifest_data, input$legend_label_selection),{
  if(input$legend_label_selection == "Analysis ID"){
    rv$manifest_legend_names <- rv$analysis_ID_legend_names
  } else {
    rv$manifest_legend_names <- rv$result_names_legend_names
  }
})

# Fix the legend names if edited
observeEvent(input$manifest_table_cell_edit,{
  if(input$legend_label_selection == "Result Name"){
    legend_names <- rv$manifest_legend_names
    current_edits <- input$manifest_table_cell_edit
    for(i in 1:length(current_edits$row)){
      legend_names[current_edits$row[i]] <- current_edits$value[i]
    }
    # Update the reactive variables
    rv$manifest_legend_names <- legend_names
    rv$result_names_legend_names <- legend_names
  }
})

################################################################################
############################## Plotting the data ###############################
################################################################################

# Select the kind of plot to show
output$type_dropdown <- renderUI({
  if(input$plot_multi_result_to_show != "all"){
    selectInput("plot_multi_type",
                "Result type",
                c("line", "TCD"))
  } else {
    selectInput("plot_multi_type",
                "Result type",
                "line")
  }
})

# If Result Name is selected, add the following text box
output$suggest_result_name_edit <- renderText({
  if(input$legend_label_selection == "Result Name"){
    "<font color='blue'> Tip: Try editing the result names in the table </font>"
  } else {
    NULL
  }
})

# Rendering the manifest plot
output$manifest_plot <- renderPlot({
  # Validate for both data and then for row selection
  validate(
    need(rv$manifest_data, "No data uploaded")
  )
  validate(
    need(input$manifest_table_rows_selected, "Select rows from table below to plot")
  )

  # Identifying the file path of the manifest file
  manifest_path <- file.path(dirname(as.character(rv$manifest_chosen)), "")

  # Grabbing the updated names for selected columns
  name_list <- rv$manifest_legend_names[input$manifest_table_rows_selected]

  # If two have the same name, then add the row index to the legend
  # input$manifest_table_rows_selected lists ind in the order selected
  unique_names <- unique(name_list)
  name_counts <- table(name_list)

  for(ind in seq_along(name_list)){
    name <- name_list[ind]
    # For more than 1 file with the same name, add the row number to the titles
    if (name_counts[name] > 1){
      new_name <- paste0(name, " (", input$manifest_table_rows_selected[ind], ")")
      name_list[ind] <- new_name
    }
  }

  # Plot results
  plot_main_results(manifest_path,
                    input$manifest_table_rows_selected,
                    results_to_show = input$plot_multi_result_to_show,
                    type = input$plot_multi_type,
                    display_names = name_list)
})

