
################################################################################
######################### Initialize reactive variables ########################
################################################################################

rv$editable_cols <- NULL
rv$result_names_legend_names <- NULL
rv$analysis_ID_legend_names <- NULL

################################################################################
############################## Load manifest data ##############################
################################################################################

# Select the manifest data
shinyFiles::shinyFileChoose(input, "manifest_data",
                            root = c(wd=file.path('results')),
                            filetypes = "rda")

# Load the selected data and save variables
observeEvent(input$manifest_data, {
  # Find and load the data of the manifest file
  manifest_path <- shinyFiles::parseFilePaths(c(wd=rv$result_base_dir),
                                              input$manifest_data)
  req(manifest_path$datapath)
  rv$manifest_chosen <- manifest_path$datapath
  load(rv$manifest_chosen)
  rv$manifest_data <- manifest_df

  # Columns to disable other than result_name
  # Hard coded only for result_name, not location of column
  # Elisa should i fix that?
  result_ind <- which(colnames(rv$manifest_data)== "result_name")
  all_cols <- 1:length(rv$manifest_data)
  rv$editable_cols <- all_cols[-c(result_ind)]

  # Saving the values of columns
  rv$result_names_legend_names <- rv$manifest_data$result_name
  rv$analysis_ID_legend_names <- rv$manifest_data$analysis_ID
})

# Show the manifest data file path
output$show_chosen_manifest <- renderText({
  if(is.integer(input$manifest_data)){
    "No manifest file chosen"
  } else {
    basename(rv$manifest_chosen)
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
  if(input$legend_label_selection == "Analysis ID"){
    temp_names <- rv$manifest_legend_names
    current_edits <- input$manifest_table_cell_edit
    for(i in 1:length(current_edits$row)){
      temp_names[current_edits$row[i]] <- current_edits$value[i]
    }
    # Update the reactive variables
    rv$manifest_legend_names <- temp_names
    rv$result_names_legend_names <- temp_names
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

  # Plot results
  plot_main_results(manifest_path,
                    input$manifest_table_rows_selected,
                    results_to_show = input$plot_multi_result_to_show,
                    type = input$plot_multi_type,
                    display_names = name_list)
})

