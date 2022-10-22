
# Reactive function that is used in multiple places

reactive_level_repetition_info_each_site <- reactive({
  req(reactive_DS_levels_to_use())
  if(input$DS_type == "ds_basic"){
    num_label_reps <- NeuroDecodeR:::get_num_label_repetitions_each_site(rv$binned_data,
                                                                         input$DS_basic___p___labels,
                                                                         label_levels = reactive_DS_levels_to_use())
  }else{
    num_label_reps <- NeuroDecodeR:::get_num_label_repetitions_each_site(rv$binned_data,
                                                                         input$DS_gen_var_to_use,
                                                                         label_levels = reactive_DS_levels_to_use())
  }
  num_label_reps
})


# DS___np___max_repetition_avail_with_any_site ----
output$DS___np___max_repetition_avail_with_any_site <- renderText({
  req(reactive_level_repetition_info_each_site())
  temp_level_rep_info <- reactive_level_repetition_info_each_site()
  paste("Levels chosen for training:", "<font color='red'>",
        paste(reactive_DS_levels_to_use(), collapse = ', '),
        "<br/>", "</font>", "The maximum number of repetitions across all the levels for training as set on the Data Source tab is",
        "<font color='red'>",
        min(temp_level_rep_info$min_repeats), "</font>", ".")
})


# ds_basic Outputs ----
## DS_basic___p___num_cv_splits ----
output$DS_basic___p___num_cv_splits = renderUI({
  req(rv$binned_file_name)
  numericInput("DS_basic___p___num_cv_splits",
               "Number of cross validation splits",
               value = 2, min = 2)

})


## DS_basic___p___num_label_repeats_per_cv_split ----
output$DS_basic___p___num_label_repeats_per_cv_split = renderUI({
  numericInput("DS_basic___p___num_label_repeats_per_cv_split",
               "Number of repeats of each level in each CV split",
               value = 1, min = 2)
})


## DS_show_chosen_repetition_info ----
output$DS_show_chosen_repetition_info <- renderText({
  req(reactive_level_repetition_info_each_site())
  temp_chosen_repetition_info <- reactive_level_repetition_info_each_site()
  num_repetitions <- input$DS_basic___p___num_label_repeats_per_cv_split * input$DS_basic___p___num_cv_splits
  num_usable_sites <- sum(temp_chosen_repetition_info$min_repeats >= num_repetitions)
  if (input$DS_type == "ds_basic"){
    paste("You selected", "<font color='red'>",
          num_repetitions, "</font>",
          "trials (", input$DS_basic___p___num_label_repeats_per_cv_split,
          " repeats x ",  input$DS_basic___p___num_cv_splits,
          "CV splits). Based on the levels selected Data source tab, this gives <font color='red'>"
          , num_usable_sites, "</font>", " sites available for decoding.")
  }else{
    paste("You selected", "<font color='red'>",
          temp_chosen_repetition_info$num_repetition, "</font>",
          "trials (", input$DS_gen___p___num_label_repeats_per_cv_split,
          " repeats x ",  input$DS_gen___p___num_cv_splits,
          "CV splits). Based on the levels selected Data source tab, this gives <font color='red'>")
    #,temp_chosen_repetition_info$num_sites_avail, "</font>", " sites available for decoding.")
  }

})


## DS_basic___p___num_resample_sites ----
output$DS_basic___p___num_resample_sites = renderUI({
  numericInput("DS_basic___p___num_resample_sites",
               "Number of resampling sites",
               value = NULL, min = 1)
})

# ds_generalization outputs
## DS_gen___p___num_cv_splits ----
output$DS_gen___p___num_cv_splits = renderUI({
  req(rv$binned_file_name)
  numericInput("DS_gen___p___num_cv_splits",
               "Number of cross validation splits",
               value = 2, min = 2)

})


## DS_gen___p___num_label_repeats_per_cv_split ----
output$DS_gen___p___num_label_repeats_per_cv_split = renderUI({
  numericInput("DS_gen___p___num_label_repeats_per_cv_split",
               "Number of repeats of each level in each CV split",
               value = 1, min = 1)
})


## DS_gen___p___num_resample_sites ----
output$DS_gen___p___num_resample_sites = renderUI({
  numericInput("DS_gen___p___num_resample_sites",
               "Number of resampling sites",
               value = NULL, min = 1)
})


## DS_show_level_repetition_info ----
output$DS_show_level_repetition_info <- renderPlotly({
  req(reactive_level_repetition_info())
  temp_level_repetition_info <- reactive_level_repetition_info()
  ggplotly(plot(temp_level_repetition_info))
})

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


