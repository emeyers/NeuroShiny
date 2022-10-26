

# DC_script_mode ----
observeEvent(input$DC_script_mode,{

  # Can't generate a script if a particular binned data file has not been selected
  req(input$DS___p___binned_data)

  rv$id <-  c("DS___p___binned_data", "DS_type")


  # Data Source

  # For ds_basic parameters
  if(input$DS_type == "ds_basic"){
    rv$id <- c(rv$id,"DS_basic___p___labels",
                    "DS_basic___p___num_label_repeats_per_cv_split", "DS_basic___p___num_cv_splits",
                    "DS_show_chosen_repetition_info", "DS_basic___p___num_resample_sites")
    if(input$DS_basic___np___select_levels){
      rv$id <- c(rv$id,  "DS_basic___p___label_levels")
    }
    if(input$DS_basic___np___advanced){
      rv$id <- c(rv$id,  "DS_basic___p___use_count_data", "DS_basic___p___site_IDs_to_use",
                      "DS_basic___p___site_IDs_to_exclude", "DS_basic___p___randomly_shuffled_labels",
                      "DS_basic___p___create_simultaneous_populations")
    }

    #ds_gen
  } else {
    rv$id <- c(rv$id, "DS_gen___np___class_number", "DS_basic___p___labels",
                    "DS_gen___p___num_cv_splits","DS_gen___p___num_label_repeats_per_cv_split",
                    "DS_gen___p___num_resample_sites")


    for (class_i in 1:input$DS_gen___np___class_number){
      rv$id <- c(rv$id,
                      paste0("DS_gen___p___train_label_levels_class_",  class_i),
                      paste0("DS_gen___p___test_label_levels_class_",  class_i))
    }

    if(input$DS_gen___np___advanced){
      rv$id <- c(rv$id, "DS_gen___p___use_count_data","DS_gen___p___site_IDs_to_use",
                      "DS_gen___p___site_IDs_to_exclude", "DS_gen___p___randomly_shuffled_labels",
                      "DS_gen___p___create_simultaneous_populations")
    }
  }

  # Classifier
  rv$id <- c(rv$id, "CL_type")
  rv$id <- c(rv$id, "CL___p___return_decision_values")
  if(input$CL_type == 'cl_svm'){
    rv$id <- c(rv$id, "CL_svm___p___kernel", "CL_svm___p___cost")
    if(input$CL_svm___p___kernel == 'polynomial'){
      rv$id <- c(rv$id, "CL_svm___p___degree", "CL_svm___p___coef0", "CL_svm___p___gamma")
    }
    if(input$CL_svm___p___kernel == 'radial'){
      rv$id <- c(rv$id, "CL_svm___p___coef0", "CL_svm___p___gamma")
    }
    if(input$CL_svm___p___kernel == 'sigmoid'){
      rv$id <- c(rv$id, "CL_svm___p___gamma")
    }
  }


  #Feature Preprocessors
  rv$id <- c(rv$id, "FP_type")
  if ('fp_select_k_features' %in% input$FP_type) {
    rv$id <- c(rv$id, "FP_skf___p___num_sites_to_use",
                    "FP_skf___p___num_sites_to_exclude")
  }


  #Result Metrics
  rv$id <- c(rv$id, "RM_type")
  if ('rm_main_results' %in% input$RM_type) {
    rv$id <- c(rv$id, "RM_mr___p___include_norm_rank_results")
  }
  if ('rm_confusion_matrix' %in% input$RM_type){
    rv$id <- c(rv$id, "RM_cm___p___save_TCD_results",
                    "RM_cm___p___create_decision_vals_confusion_matrix")
  }

  #Cross Validator
  rv$id <- c(rv$id, "CV_standard___p___run_TCD",
                  "CV_standard___p___num_resample_runs")

  if(!is.null(input$CV_standard___p___num_parallel_cores) &&
     !is.na(input$CV_standard___p___num_parallel_cores) &&
     input$CV_standard___p___num_parallel_cores >= 1) {
    rv$id <- c(rv$id, "CV_standard___p___num_parallel_cores",
                    "CV_standard___p___parallel_outfile")
  }

  rv$id <- c(rv$id, "include_comments")

  rv$inputIDs <- paste0("input$", rv$id)
  rv$values <- lapply(rv$inputIDs, function(i){
    eval(str2lang(i))
  })

  decoding_params <- rv$values
  decoding_params <- setNames(decoding_params, rv$id)


  # add the directory name of the binned data to the decoding params
  decoding_params$working_dir <- working_dir
  decoding_params$binned_dir_name <- rv$binned_base_dir
  decoding_params$results_dir_name <- rv$result_base_dir #might break this


  #decoding_params$include_comments <- FALSE

  if (input$DC_script_mode == "R") {
    rv$displayed_script <- generate_r_script_from_shiny_decoding_params(decoding_params)
  } else if (input$DC_script_mode == "R Markdown") {
    rv$displayed_script <- generate_r_markdown_from_shiny_decoding_params(decoding_params)
  }


  rv$displayed_script


})


# include_comments ----
# Trying to get the script to update when the include comments box is
# checked/unchecked but this is not working :(

observeEvent(input$include_comments,{


  # unfortunately call this function does nothing so need to manually write the code to do this
  # update_ace_editor_code()

  curr_radio_button_setting <- input$DC_script_mode

  updateRadioButtons(session, "DC_script_mode", "File type for generated script",
                     c("R", "R Markdown", "Matlab"), selected = "R Markdown")

  updateRadioButtons(session, "DC_script_mode", "File type for generated script",
                     c("R", "R Markdown", "Matlab"), selected = "R")

  updateRadioButtons(session, "DC_script_mode", "File type for generated script",
                     c("R", "R Markdown", "Matlab"), selected = curr_radio_button_setting)

})


# DC_offer_scriptize ----
output$DC_offer_scriptize = renderUI({
  list(
    actionButton("DC_run_script", "Run and save the script"),
    uiOutput("DC_scriptize_error")
  )
})

observeEvent(input$DC_run_script,{
  req(rv$displayed_script)

  # Generate file name
  script_file_name <- generate_script_name(input$DC_script_mode,
                                           result_base_dir, script_save_dir)
  rv$save_script_name <- script_file_name

  # Write the code to a script
  fileConn <- file(script_file_name)
  writeLines(rv$displayed_script, fileConn)
  close(fileConn)

  rv$pdf_knitting_status <- "running"
  # Run the script
  if (input$DC_script_mode == "R") {
    #source(script_file_name)
    pdf_file_name <- stringr::str_replace(script_file_name, "r_scripts", "r_scripts_pdf")
    pdf_file_name <- stringr::str_replace(pdf_file_name, "R$", "pdf")
  } else if (input$DC_script_mode == "R Markdown") {
    pdf_file_name <- stringr::str_replace(script_file_name, "r_markdown", "r_markdown_pdf")
    pdf_file_name <- stringr::str_replace(pdf_file_name, "Rmd$", "pdf")
  }


  rv$latest_pdf_file_name <- basename(pdf_file_name)

  # Add a notification that the document is knitting
  running_id <- showNotification("Compiling results...",
                                 duration = NULL,
                                 closeButton = FALSE,
                                 type = "message")
  on.exit(removeNotification(running_id), add = TRUE)

  rmarkdown::render(script_file_name, "pdf_document", output_dir = dirname(pdf_file_name))


  rv$pdf_knitting_status <- "completed"
  file.copy(pdf_file_name, paste0("www/", basename(pdf_file_name)))

})

output$DC_scriptize_error <- renderText({
  # To do, fix
  #er_scriptize_action_error()
})

er_scriptize_action_error <- eventReactive(rv$decoding_para_id_computed,{
  # if we don't have this line, this function will be called as soon as users
  #click the script tab because rv$decoding_para_id_computed is going
  #from NULL to 1 (I think)
  req(rv$id)
  validate(
    need(input$DS___p___binned_data, "No data has been uploaded")
  )
  temp_need = lapply(rv$id, function(i){
    eval(parse(text = paste0("need(input$", i, ", '", "You need to set your parameter first')")))
  })
  do.call(validate, temp_need)
})



# DC_offer_save_decoding ----
output$DC_offer_save_decoding = renderUI({
  list(
    helpText(""),
    actionButton("DC_save_decoding", "Save the script only"),
    uiOutput("DC_save_decoding_error")
  )
})

observeEvent(input$DC_save_decoding, {
  req(rv$displayed_script)
  # Generate script name
  script_file_name <- generate_script_name(input$DC_script_mode,
                                           result_base_dir, script_save_dir)
  rv$save_script_name <- script_file_name

  # Write the code to a script
  fileConn <- file(script_file_name)
  writeLines(rv$displayed_script, fileConn)
  close(fileConn)
})

output$DC_save_decoding_error = renderUI({
  er_DC_save_displayed_script_error()
})

er_DC_save_displayed_script_error <- eventReactive(input$DC_save_displayed_script,{
  validate(
    need(rv$displayed_script,"Please generate the script first"),
    need(input$DC_to_be_saved_script_name, "Please tell me the new script name")
  )
})

# DC_plot_pdf ----
output$DC_plot_pdf <- renderUI({
  # Elisa, missing the two rvs
  if (!is.null(rv$pdf_knitting_status)) {
    tags$iframe(style="height:0px; width:0%; scrolling = yes",
                src = rv$latest_pdf_file_name)
  }
})


# DC_ace ----
output$DC_ace = renderUI({

  if (input$DC_script_mode == "Matlab") {
    script_editor_mode <- "matlab"
  } else {
    script_editor_mode <- "r"
  }

  ace_editor <- shinyAce::aceEditor("script",
                                    rv$displayed_script,
                                    mode = script_editor_mode)
  update_ace_editor_code()
  ace_editor
})

# a helper function that refreshes what is on the ace editor
update_ace_editor_code <- function() {

  if (rv$displayed_script == "") {
    curr_radio_button_setting <- input$DC_script_mode

    # to make sure the editor shows the script the first time one clicks on a tab
    #  one needs to chance the radio button to a different choice and then back again
    #  (definitely a bit of a hack but it seems to work fairly well)

    updateRadioButtons(session, "DC_script_mode", "File type for generated script",
                       c("R", "R Markdown", "Matlab"), selected = "R Markdown")

    updateRadioButtons(session, "DC_script_mode", "File type for generated script",
                       c("R", "R Markdown", "Matlab"), selected = "R")

    updateRadioButtons(session, "DC_script_mode", "File type for generated script",
                       c("R", "R Markdown", "Matlab"), selected = curr_radio_button_setting)

  }

}

