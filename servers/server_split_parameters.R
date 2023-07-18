
################################################################################
########################## Levels chosen for training ##########################
################################################################################

# Find the levels to use
reactive_DS_levels_to_use <- reactive({
  req(rv$binned_data)
  # For basic data sources
  if(input$DS_type == "ds_basic") {
    validate(need(!is.null(input$DS_basic___p___label_levels)||!input$DS_basic___np___select_levels, paste0("You haven't set your levels yet")))
    # If no selected
    if(!input$DS_basic___np___select_levels) {
      reactive_all_basic_levels_to_use()
    } else {
      input$DS_basic___p___label_levels
    }
    # For generalization data sources
  } else {
    all_labels <- c()
    for (class_i in 1:input$DS_gen___np___class_number) {
      curr_train_labels <- eval(str2lang(paste0("input$DS_gen___p___train_label_levels_class_", class_i)))
      curr_test_labels <- eval(str2lang(paste0("input$DS_gen___p___test_label_levels_class_", class_i)))
      all_labels <- unique(c(all_labels, curr_train_labels, curr_test_labels))
    }
    all_labels
  }
})

# Finding the maximum levels of repetitions across all levels to be used
reactive_level_repetition_info_each_site <- reactive({
  req(reactive_DS_levels_to_use())
  # For DS basic
  if(input$DS_type == "ds_basic"){
    num_label_reps <- NeuroDecodeR:::get_num_label_repetitions_each_site(rv$binned_data,
                                                                         input$DS_basic___p___labels,
                                                                         label_levels = reactive_DS_levels_to_use())
    # For DS generalization
  } else {
    num_label_reps <- NeuroDecodeR:::get_num_label_repetitions_each_site(rv$binned_data,
                                                                         input$DS_gen___p___labels,
                                                                         label_levels = reactive_DS_levels_to_use())
  }
  num_label_reps
})

# Display the levels to use and
# Display the maximum levels of repetitions across all of them
output$DS___np___max_repetition_avail_with_any_site <- renderText({
  req(reactive_level_repetition_info_each_site())
  temp_level_rep_info <- reactive_level_repetition_info_each_site()
  paste("Levels chosen for training:", "<font color='red'>",
        paste(reactive_DS_levels_to_use(), collapse = ', '),
        "<br/>", "</font>", "The maximum number of repetitions across all the levels for training as set on the Data Source tab is",
        "<font color='red'>",
        min(temp_level_rep_info$min_repeats), "</font>", ".")
})

################################################################################
######################### Cross-validation splits ##############################
################################################################################

# DS basic input box for the number of cross-validation splits
output$DS_basic___p___num_cv_splits <- renderUI({
  req(rv$binned_file_name)
  numericInput("DS_basic___p___num_cv_splits",
               "Number of cross validation splits",
               value = 2, min = 2)

})

# DS generalization input box for the number of cross-validation splits
output$DS_gen___p___num_cv_splits <- renderUI({
  req(rv$binned_file_name)
  numericInput("DS_gen___p___num_cv_splits",
               "Number of cross validation splits",
               value = 2, min = 2)

})

################################################################################
#################### Repeats of each level in each CV split ####################
################################################################################

# DS basic input box for the number of repeats
output$DS_basic___p___num_label_repeats_per_cv_split <- renderUI({
  numericInput("DS_basic___p___num_label_repeats_per_cv_split",
               "Number of repeats of each level in each CV split",
               value = 1, min = 2)
})

# DS generalization input box for the number of repeats
output$DS_gen___p___num_label_repeats_per_cv_split <- renderUI({
  numericInput("DS_gen___p___num_label_repeats_per_cv_split",
               "Number of repeats of each level in each CV split",
               value = 1, min = 1)
})

################################################################################
########################## Number of resampling sites ##########################
################################################################################
# DS basic reactive variable for resampling sites
reactive_num_repetitions <- reactive({
  num_repetitions <- input$DS_basic___p___num_label_repeats_per_cv_split * input$DS_basic___p___num_cv_splits
  num_repetitions
})

reactive_num_usable_sites <- reactive({
  req(reactive_level_repetition_info_each_site())
  temp_chosen_rep_info <- reactive_level_repetition_info_each_site()
  num_usable_sites <- sum(temp_chosen_rep_info$min_repeats >= reactive_num_repetitions())
  num_usable_sites
})

# Display number of trials and sites available for decoding
# Function reactive_level_repetition_info_each_site() in above section
output$DS_show_chosen_repetition_info <- renderText({
  req(reactive_level_repetition_info_each_site())
  if (input$DS_type == "ds_basic"){
    paste("You selected", "<font color='red'>",
          reactive_num_repetitions(), "</font>",
          "trials (", input$DS_basic___p___num_label_repeats_per_cv_split,
          " repeats x ",  input$DS_basic___p___num_cv_splits,
          "CV splits). Based on the levels selected Data source tab, this gives <font color='red'>",
          reactive_num_usable_sites(), "</font>",
          " sites available for decoding.")
  }else{
    paste("You selected", "<font color='red'>",
          temp_chosen_repetition_info$num_repetition, "</font>",
          "trials (", input$DS_gen___p___num_label_repeats_per_cv_split,
          " repeats x ",  input$DS_gen___p___num_cv_splits,
          "CV splits). Based on the levels selected Data source tab, this gives <font color='red'>")
    #,temp_chosen_repetition_info$num_sites_avail, "</font>", " sites available for decoding.")
  }

})

# Running get_num_label_repetitions() to be used below
reactive_level_repetition_info <- reactive({
  req(reactive_DS_levels_to_use())
  if(input$DS_type == "ds_basic"){
    num_label_reps <- NeuroDecodeR:::get_num_label_repetitions(rv$binned_data,
                                                               input$DS_basic___p___labels,
                                                               label_levels = reactive_DS_levels_to_use())
  }else{
    #TO DO what is this for gen?
    num_label_reps <- NeuroDecodeR:::get_num_label_repetitions(rv$binned_data,
                                                               input$DS_gen___p___labels,
                                                               label_levels = reactive_DS_levels_to_use())
  }
  num_label_reps
})

# DS basic input the number of resampling sites
output$DS_basic___p___num_resample_sites <- renderUI({
  numericInput("DS_basic___p___num_resample_sites",
               "Number of resampling sites",
               value = NULL, min = 1, max = reactive_num_usable_sites())
})

# DS generalization input the number of resampling sites
output$DS_gen___p___num_resample_sites <- renderUI({
  numericInput("DS_gen___p___num_resample_sites",
               "Number of resampling sites",
               value = NULL, min = 1) # Elisa
})

# Plot the repetions
output$DS_show_level_repetition_info <- renderPlotly({
  req(reactive_level_repetition_info())
  temp_level_repetition_info <- reactive_level_repetition_info()
  ggplotly(plot(temp_level_repetition_info))
})


