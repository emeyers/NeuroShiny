rv$editable_cols <- NULL


rv$result_names_legend_names <- NULL
rv$analysis_ID_legend_names <- NULL


################################################################################
################################ manifest_data #################################

# manifest_data ----
shinyFiles::shinyFileChoose(input, "manifest_data",
                            root = c(wd=file.path('.', 'results')),
                            filetypes = "rda")

# When data is uploaded, save path, load in, and save reactiveValues
observeEvent(input$manifest_data,{
  manifest_path <- shinyFiles::parseFilePaths(c(wd= rv$result_base_dir),
                                             input$manifest_data)
  req(manifest_path$datapath)
  rv$manifest_chosen <- manifest_path$datapath
  load(rv$manifest_chosen)
  rv$manifest_data <- manifest_df

  # Columns to disable other than result_name
  # Hard coded only for result_name, not location of column
  result_ind <- which(colnames(rv$manifest_data)== "result_name")
  analysis_ind <- which(colnames(rv$manifest_data)== "analysis_ID")
  all_cols <- 1:length(rv$manifest_data)
  rv$editable_cols <- all_cols[-c(result_ind,analysis_ind)]

  # Saving the values of columns
  rv$result_names_legend_names <- rv$manifest_data$result_name
  rv$analysis_ID_legend_names <- rv$manifest_data$analysis_ID
})

# show_chosen_manifest ----
output$show_chosen_manifest <- renderText({
  if(is.integer(input$manifest_data)){
    "No manifest file chosen"
  } else{
    basename(rv$manifest_chosen)
  }
})


output$manifest_table <- renderDT({
  req(rv$manifest_data)

  DT::datatable(rv$manifest_data,
                #editable = TRUE,
                editable = list(target = "cell",
                                disable = list(columns = rv$editable_cols)),
                #editable = list(target = "column",
                #                disable = list(columns = rv$editable_cols)),
                options = list(scrollX = TRUE))
})

################################################################################
############################### plot_parameters ################################

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


# When the data changes, or the legend label selection changes:
# assign the current editable column and legend names accordingly
observeEvent(list(input$manifest_data, input$legend_label_selection),{
  if(input$legend_label_selection == "Analysis ID"){
    rv$manifest_legend_names <- rv$analysis_ID_legend_names
  }
  else{
    rv$manifest_legend_names <- rv$result_names_legend_names
  }
})

################################################################################
################################ manifest_plot #################################

output$manifest_plot <- renderPlot({
  # Validate for both data and then for row selection
  validate(
    need(rv$manifest_data, "No data uploaded")
  )
  validate(
    need(input$manifest_table_rows_selected, "Select rows from table below to plot")
  )


  # Identifying the file path of the manifest file
  manifest_path <- file.path(dirname(as.character(rv$manifest_chosen)),"")

  # Grabbing the updated names for selected columns
  name_list <- rv$manifest_legend_names[input$manifest_table_rows_selected]

  # Plot results
  plot_main_results(manifest_path,
                    input$manifest_table_rows_selected,
                    results_to_show = input$plot_multi_result_to_show,
                    type = input$plot_multi_type,
                    display_names = name_list)



})

# When names are edited, fix the legend names
observeEvent(input$manifest_table_cell_edit,{
  temp_names <- rv$manifest_legend_names
  current_edits <- input$manifest_table_cell_edit
  for(i in 1:length(current_edits$row)){
    temp_names[current_edits$row[i]] <- current_edits$value[i]
  }
  rv$manifest_legend_names <- temp_names

  if(input$legend_label_selection == "Analysis ID"){
    rv$analysis_ID_legend_names <- temp_names
  }
  else{
    rv$result_names_legend_names <- temp_names
  }

})



