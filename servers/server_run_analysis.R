
################################################################################
############################ Update displayed script ###########################
################################################################################

# Update only if those in list have been changed
# The variable input$decoding_tabs is the id value of the tab panel in ui.R
# The result is any change from those tabs will be added since it observes
# events when the user navigates between them
observeEvent(list(input$decoding_tabs,
                  input$DC_script_mode,
                  input$include_comments,
                  input$result_name), {

  # Requires a binned data to update and add to script with ds_type
  req(input$DS___p___binned_data)
  rv$script <- c("DS___p___binned_data", "DS_type")


  # DATA SOURCE
  # Add the parameters for ds_basic
  if(input$DS_type == "ds_basic"){
    # Required parameters for ds_basic
    rv$script <- c(rv$script,"DS_basic___p___labels",
                   "DS_basic___p___num_label_repeats_per_cv_split",
                   "DS_basic___p___num_cv_splits",
                   "DS_basic___p___num_resample_sites")
    # If labels selected
    if(input$DS_basic___np___select_levels){
      rv$script <- c(rv$script, "DS_basic___p___label_levels")
    }
    # Additional parameters for ds_basic
    if(input$DS_basic___np___advanced){
      rv$script <- c(rv$script, "DS_basic___p___use_count_data",
                     "DS_basic___p___site_IDs_to_use",
                     "DS_basic___p___site_IDs_to_exclude",
                     "DS_basic___p___randomly_shuffled_labels",
                     "DS_basic___p___create_simultaneous_populations")
    }
  # Add parameters for ds_gen
  } else {
    rv$script <- c(rv$script,
                   "DS_gen___np___class_number",
                   "DS_basic___p___labels",
                   "DS_gen___p___num_cv_splits",
                   "DS_gen___p___num_label_repeats_per_cv_split",
                   "DS_gen___p___num_resample_sites")
    # Adding classes
    for (class_i in 1:input$DS_gen___np___class_number){
      rv$script <- c(rv$script,
                     paste0("DS_gen___p___train_label_levels_class_",  class_i),
                     paste0("DS_gen___p___test_label_levels_class_",  class_i))
    }
    # Additional parameters for ds_gen
    if(input$DS_gen___np___advanced){
      rv$script <- c(rv$script,
                     "DS_gen___p___use_count_data",
                     "DS_gen___p___site_IDs_to_use",
                     "DS_gen___p___site_IDs_to_exclude",
                     "DS_gen___p___randomly_shuffled_labels",
                     "DS_gen___p___create_simultaneous_populations")
    }
  }


  # CLASSIFIER
  # Required classifier parameters
  rv$script <- c(rv$script, "CL_type", "CL___p___return_decision_values")
  # Add parameters if selected type is SVM
  if(input$CL_type == 'cl_svm'){
    # General SVM
    rv$script <- c(rv$script, "CL_svm___p___kernel", "CL_svm___p___cost")
    # Polynomial kernel parameters
    if(input$CL_svm___p___kernel == 'polynomial'){
      rv$script <- c(rv$script, "CL_svm___p___degree",
                     "CL_svm___p___coef0", "CL_svm___p___gamma")
    }
    # Radial kernel parameters
    if(input$CL_svm___p___kernel == 'radial'){
      rv$script <- c(rv$script, "CL_svm___p___coef0", "CL_svm___p___gamma")
    }
    # Sigmoid kernel parameters
    if(input$CL_svm___p___kernel == 'sigmoid'){
      rv$script <- c(rv$script, "CL_svm___p___gamma")
    }
  }


  # FEATURE PREPROCESSORS
  # Required parameters
  rv$script <- c(rv$script, "FP_type")
  # Add additional parameters if k features is selected
  if ('fp_select_k_features' %in% input$FP_type) {
    rv$script <- c(rv$script, "FP_skf___p___num_sites_to_use",
                   "FP_skf___p___num_sites_to_exclude")
  }


  # RESULT METRICS
  # Required parameters
  rv$script <- c(rv$script, "RM_type")
  # Add additional parameters if main results is selected
  if ('rm_main_results' %in% input$RM_type) {
    rv$script <- c(rv$script, "RM_mr___p___include_norm_rank_results")
  }
  # Add additional parameters if confusion matrix is selected
  if ('rm_confusion_matrix' %in% input$RM_type){
    rv$script <- c(rv$script, "RM_cm___p___save_TCD_results",
                   "RM_cm___p___create_decision_vals_confusion_matrix")
  }


  # CROSS VALIDATOR
  # Add standard cv items
  rv$script <- c(rv$script, "CV_standard___p___run_TCD",
                 "CV_standard___p___num_resample_runs")
  # Elisa - why all three?
  # If there is 1 or more parallel cores, add corresponding variables
  if(!is.null(input$CV_standard___p___num_parallel_cores) &&
     !is.na(input$CV_standard___p___num_parallel_cores) &&
     input$CV_standard___p___num_parallel_cores >= 1) {
    rv$script <- c(rv$script, "CV_standard___p___num_parallel_cores",
                   "CV_standard___p___parallel_outfile")
  }


  # COMMENTS & FILE NAME
  rv$script <- c(rv$script, "include_comments", "result_name")


  # PREPARE SCRIPT VALUES
  # Add string input$ to all the values
  rv$inputIDs <- paste0("input$", rv$script)
  # Evaluating these inputs
  rv$values <- lapply(rv$inputIDs, function(i){
    eval(str2lang(i))
  })
  # Create a new list with the values generated by evaluating the inputs
  decoding_params <- rv$values
  # Add names to the list
  decoding_params <- setNames(decoding_params, rv$script)
  # Add directory names to the decoding params
  decoding_params$binned_dir_name <- rv$binned_base_dir
  decoding_params$results_dir_name <- rv$result_base_dir


  # GENERATE SCRIPT
  if (input$DC_script_mode == "R") {
    rv$displayed_script <- generate_r_script_from_shiny_decoding_params(decoding_params)
  } else if (input$DC_script_mode == "R Markdown") {
    rv$displayed_script <- generate_r_markdown_from_shiny_decoding_params(decoding_params)
  }
  # Display the updated script
  rv$displayed_script

})

################################################################################
############################## Run and save script #############################
################################################################################

# Button to run and save
output$DC_offer_scriptize <- renderUI({
  list(
    actionButton("DC_run_script", "Run and save the script"),
    uiOutput("DC_scriptize_error") # elisa this still doesn't work it's below
  )
})


# Running the script
observeEvent(input$DC_run_script,{
  req(rv$displayed_script)

  # Generate file name
  script_file_name <- generate_script_name(input$DC_script_mode,
                                           rv$result_base_dir, script_save_dir)
  rv$save_script_name <- script_file_name

  # Write the code to a script
  fileConn <- file(script_file_name)
  writeLines(rv$displayed_script, fileConn)
  close(fileConn)

  rv$pdf_knitting_status <- "running"
  # Run the script
  if (input$DC_script_mode == "R") {
    pdf_file_name <- stringr::str_replace(script_file_name, "r_scripts", "r_scripts_pdf")
    pdf_file_name <- stringr::str_replace(pdf_file_name, "R$", "pdf")
  } else if (input$DC_script_mode == "R Markdown") {
    pdf_file_name <- stringr::str_replace(script_file_name,
                                          "r_markdown", "r_markdown_pdf")
    pdf_file_name <- stringr::str_replace(pdf_file_name, "Rmd$", "pdf")
  }

  # Save pdf file name as reactive variables
  rv$latest_pdf_file_name <- basename(pdf_file_name)

  # Add a notification that the document is knitting
  running_id <- showNotification("Compiling results...",
                                 duration = NULL,
                                 closeButton = FALSE,
                                 type = "message")
  on.exit(removeNotification(running_id), add = TRUE) # elisa - what does this do

  # Render the pdf document using given output directory
  rmarkdown::render(script_file_name, "pdf_document", output_dir = dirname(pdf_file_name))

  # Update status of knitting and save to directory
  rv$pdf_knitting_status <- "completed"
  file.copy(pdf_file_name, file.path(app_base_dir, "www", basename(pdf_file_name)))

})

# Error messages when encountering error in scriptizing
output$DC_scriptize_error <- renderText({
  # elisa To do, fix
  #er_scriptize_action_error()
})

er_scriptize_action_error <- eventReactive(rv$decoding_para_id_computed,{
  # elisa if we don't have this line, this function will be called as soon as users
  #click the script tab because rv$decoding_para_id_computed is going
  #from NULL to 1 (I think)
  req(rv$script)
  validate(
    need(input$DS___p___binned_data, "No data has been uploaded")
  )
  temp_need = lapply(rv$script, function(i){
    eval(parse(text = paste0("need(input$", i, ", '", "You need to set your parameter first')")))
  })
  do.call(validate, temp_need)
})



################################################################################
################################ Save script only ##############################
################################################################################

# Action button for saving the file only
# elisa - maybe you don't need both, you can do it in one?
output$DC_offer_save_decoding <- renderUI({
  list(
    helpText(""),
    actionButton("DC_save_decoding", "Save the script only"),
    uiOutput("DC_save_decoding_error")
  )
})

# If the save only button is pressed, then save the script
observeEvent(input$DC_save_decoding, {
  req(rv$displayed_script)
  # Generate script name
  script_file_name <- generate_script_name(input$DC_script_mode,
                                           rv$result_base_dir, script_save_dir)
  rv$save_script_name <- script_file_name

  # Write the code to a script and save
  fileConn <- file(script_file_name)
  writeLines(rv$displayed_script, fileConn)
  close(fileConn)
})

# Error messages when encountering error in scriptizing
# Elisa - if you do end up using this maybe combine with the one above
output$DC_save_decoding_error <- renderUI({
  er_DC_save_displayed_script_error()
})

# Elisa - same deal with this one
er_DC_save_displayed_script_error <- eventReactive(input$DC_save_displayed_script,{
  validate(
    need(rv$displayed_script,"Please generate the script first"),
    need(input$DC_to_be_saved_script_name, "Please tell me the new script name")
  )
})

################################################################################
############################# Displaying the script ############################
################################################################################

# Showing the script as a pdf
output$DC_plot_pdf <- renderUI({
  # Elisa, missing the two rvs
  if (!is.null(rv$pdf_knitting_status)) {
    tags$iframe(style="height:0px; width:0%; scrolling = yes",
                src = rv$latest_pdf_file_name)
  }
})

# Showing the script in the UI
# Elisa - rename
output$DC_ace <- renderUI({
  # Depending on the code format - Elisa this still needs to be written
  if (input$DC_script_mode == "Matlab") {
    script_editor_mode <- "matlab"
  } else {
    script_editor_mode <- "r"
  }

  # Display the script
  ace_editor <- shinyAce::aceEditor("script",
                                    rv$displayed_script,
                                    mode = script_editor_mode)
  update_ace_editor_code()
  ace_editor
})

# A helper function that refreshes what is on the ace editor
# Elisa - check this cuz i don't know if we need it


# to make sure the editor shows the script the first time one clicks on a tab
#  one needs to chance the radio button to a different choice and then back again
#  (definitely a bit of a hack but it seems to work fairly well)

update_ace_editor_code <- function() {
  if (rv$displayed_script == "") {
    curr_radio_button_setting <- input$DC_script_mode
    updateRadioButtons(session, "DC_script_mode", "File type for generated script",
                       c("R", "R Markdown", "Matlab"), selected = curr_radio_button_setting)
  }
}

