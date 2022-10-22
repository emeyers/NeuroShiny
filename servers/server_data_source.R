
# DS___p___binned_data ----
shinyFiles::shinyFileChoose(input, "DS___p___binned_data",
                            roots = c(wd=binned_base_dir),
                            filetypes = "Rda")

observe({
  req(input$DS___p___binned_data)
  temp_df_file <- shinyFiles::parseFilePaths(c(wd= rv$binned_base_dir),
                                             input$DS___p___binned_data)
  req(temp_df_file$datapath)
  rv$binned_file_name <- temp_df_file$datapath

  load(rv$binned_file_name)
  rv$binned_data <- binned_data
  rv$binned_maximum_num_of_levels_in_all_var <-
    max(apply(select(binned_data, starts_with("labels"))[,],2, function(x) length(levels(as.factor(x)))))
  rv$binned_all_var <- sub("labels.", "", names(select(binned_data, starts_with("labels"))))
})


# DS_show_chosen_bin ----
output$DS_show_chosen_bin = renderText({
  if(is.na(rv$binned_file_name)){
    "No file chosen yet"
  }else{
    basename(rv$binned_file_name)
  }
})

### Basic Outputs ----
# DS_basic___p___list_of_labels ----
output$DS_basic___p___list_of_labels = renderUI({
  req(rv$binned_file_name)
  selectInput("DS_basic___p___labels",
              "Variable to decode and to use",
              rv$binned_all_var)
})


#DS_basic___np___list_of_levels_to_use ----
output$DS_basic___np___list_of_levels_to_use = renderUI({
  selectInput("DS_basic___p___label_levels",
              "Levels to use",
              reactive_all_levels_of_basic_labels(),
              multiple = TRUE)
})

reactive_all_levels_of_basic_labels <- reactive({
  req(rv$binned_file_name)
  binned_data = rv$binned_data
  levels(factor(binned_data[[paste0("labels.",input$DS_basic___p___labels)]]))
})


# DS_basic___p___use_count_data ----
output$DS_basic___p___use_count_data = renderUI({
  selectInput("DS_basic___p___use_count_data",
              "Convert the data into spike counts",
              c(FALSE, TRUE))
})


# DS_basic___p___site_IDs_to_use ----
output$DS_basic___p___site_IDs_to_use = renderUI({
  selectInput("DS_basic___p___site_IDs_to_use",
              "Which sites should be used",
              reactive_all_basic_site_IDs_to_use(),
              multiple = TRUE)
})

reactive_all_basic_site_IDs_to_use <- reactive({
  req(rv$binned_file_name)
  binned_data = rv$binned_data
  levels(unique(factor(binned_data$siteID)))
})


# DS_basic___p___site_IDs_to_exclude ----
output$DS_basic___p___site_IDs_to_exclude = renderUI({
  selectInput("DS_basic___p___site_IDs_to_exclude",
              "Which sites should be excluded",
              reactive_all_basic_site_IDs_to_exclude(),
              multiple = TRUE)
})

reactive_all_basic_site_IDs_to_exclude <- reactive({
  req(rv$binned_file_name)
  binned_data = rv$binned_data
  tempLevels <- levels(unique(factor(binned_data$siteID)))
  if (is.null(input$DS_basic___p___site_IDs_to_use)){
    tempLevels
  }else{
    tempLevels[-(which(tempLevels %in% input$DS_basic___p___site_IDs_to_use))]
  }

})


# DS_basic___p___randomly_shuffled_labels ----
output$DS_basic___p___randomly_shuffled_labels = renderUI({
  selectInput("DS_basic___p___randomly_shuffled_labels",
              "Randomly shuffle labels",
              c(FALSE, TRUE))
})


# DS_basic___p___create_simultaneous_populations ----
output$DS_basic___p___create_simultaneous_populations = renderUI({
  selectInput("DS_basic___p___create_simultaneous_populations",
              "Was the data created simultaneously?",
              c(0,1))
})

### General Outputs ----
# DS_gen___np___list_of_labels ----
output$DS_gen___np___list_of_labels = renderUI({
  req(rv$binned_file_name)
  selectInput("DS_gen___p___labels",
              "Variable to decode and to use",
              rv$binned_all_var)
})


# DS_gen___np___class_number ----
rv$prev_bins <- NULL
rv$gen_bins <- NULL

# Append new value to previous values when input$DS_gen___np___class_number changes
observeEvent(input$DS_gen___np___class_number, {
  rv$prev_bins <- c(tail(rv$prev_bins, 1), input$DS_gen___np___class_number)
})


# DS_gen___p___label_levels ----
output$DS_gen___p___label_levels = renderUI({

  req(rv$binned_file_name)
  req(input$DS_gen___np___class_number)

  train_lst <- list()
  test_lst <- list()
  for (i in 1:input$DS_gen___np___class_number){

    train_lst[[i]] <- selectInput(paste0("DS_gen___p___train_label_levels_class_", i),
                                  paste("Class",i, "- Training levels to use"),
                                  reactive_all_levels_of_gen_var_to_use(),
                                  multiple = TRUE)
    test_lst[[i]] <- selectInput(paste0("DS_gen___p___test_label_levels_class_", i),
                                 paste("Class",i, "- Testing levels to use"),
                                 reactive_all_levels_of_gen_var_to_use(),
                                 multiple = TRUE)
  }

  c(rbind(train_lst, test_lst))
})

reactive_all_levels_of_gen_var_to_use <- reactive({
  req(rv$binned_file_name)
  binned_data = rv$binned_data
  levels(factor(binned_data[[paste0("labels.",input$DS_gen___p___labels)]]))
})


# DS_gen___p___use_count_data ----
output$DS_gen___p___use_count_data = renderUI({
  selectInput("DS_gen___p___use_count_data",
              "Convert the data into spike counts",
              c(FALSE, TRUE))
})


# DS_gen___p___site_IDs_to_use ----
output$DS_gen___p___site_IDs_to_use = renderUI({
  selectInput("DS_gen___p___site_IDs_to_use",
              "Which sites should be used",
              reactive_all_gen_site_IDs_to_use(),
              multiple = TRUE)
})

reactive_all_gen_site_IDs_to_use <- reactive({
  req(rv$binned_file_name)
  binned_data = rv$binned_data
  levels(unique(factor(binned_data$siteID)))
})


# DS_gen___p___site_IDs_to_exclude ----
output$DS_gen___p___site_IDs_to_exclude = renderUI({
  selectInput("DS_gen___p___site_IDs_to_exclude",
              "Which sites should be excluded",
              reactive_all_gen_site_IDs_to_exclude(),
              multiple = TRUE)
})

reactive_all_gen_site_IDs_to_exclude <- reactive({
  req(rv$binned_file_name)
  binned_data = rv$binned_data
  tempLevels <- levels(unique(factor(binned_data$siteID)))

  if (is.null(input$DS_gen___p___site_IDs_to_use)){
    tempLevels
  }else{
    tempLevels[-(which(tempLevels %in% input$DS_gen___p___site_IDs_to_use))]
  }
})


# DS_gen___p___randomly_shuffled_labels ----
output$DS_gen___p___randomly_shuffled_labels = renderUI({
  selectInput("DS_gen___p___randomly_shuffled_labels",
              "Randomly shuffle labels",
              c(FALSE, TRUE))
})



# DS_gen___p___create_simultaneous_populations ----
output$DS_gen___p___create_simultaneous_populations = renderUI({
  selectInput("DS_basic___p___create_simultaneous_populations",
              "Was the data created simultaneously?",
              c(0,1))
})







# Reactive values that come from DS_type selection ----
reactive_DS_levels_to_use <- reactive({
  req(rv$binned_data)
  if(input$DS_type == "ds_basic"){
    validate(
      need(!is.null(input$DS_basic___p___label_levels)||!input$DS_basic___np___select_levels, paste0("You haven't set your levels yet")))

    if(!input$DS_basic___np___select_levels){
      reactive_all_levels_of_basic_labels()
    } else {
      input$DS_basic___p___label_levels
    }
  } else {
    all_labels <- c()
    for (class_i in 1:input$DS_gen___np___class_number) {
      curr_train_labels <- eval(str2lang(paste0("input$DS_gen___p___train_label_levels_class_", class_i)))
      curr_test_labels <- eval(str2lang(paste0("input$DS_gen___p___test_label_levels_class_", class_i)))
      all_labels <- c(all_labels, curr_train_labels, curr_test_labels)
    }
    all_labels
  }  # end else statement for the ds_generalization
})

reactive_chosen_repetition_info <- reactive({
  if(input$DS_type == "ds_basic"){
    req(input$DS_basic___p___num_cv_splits, input$DS_basic___p___num_label_repeats_per_cv_split, reactive_level_repetition_info())
    temp_level_repetition_info <- reactive_level_repetition_info()
    list(num_repetition = input$DS_basic___p___num_label_repeats_per_cv_split * input$DS_basic___p___num_cv_splits,
         num_sites_avail = nrow(filter(temp_level_repetition_info, min_repeats >= input$DS_basic___p___num_label_repeats_per_cv_split * input$DS_basic___p___num_cv_splits)))

  }else{
    req(input$DS_gen___p___num_cv_splits, input$DS_gen___p___num_label_repeats_per_cv_split, reactive_level_repetition_info())
    temp_level_repetition_info <- reactive_level_repetition_info()
    list(num_repetition = input$DS_gen___p___num_label_repeats_per_cv_split * input$DS_gen___p___num_cv_splits,
         num_sites_avail = nrow(filter(temp_level_repetition_info, min_repeats >= input$DS_gen___p___num_label_repeats_per_cv_split * input$DS_gen___p___num_cv_splits)))
  }
})








