
################################################################################
############################## Select binned data ##############################
################################################################################

# Binned data button
shinyFiles::shinyFileChoose(input, "DS___p___binned_data",
                            roots = c(wd=file.path('.', 'data','binned')),
                            filetypes = "Rda")

# Load in data to find variable labels
observe({
  # Create file path
  req(input$DS___p___binned_data)
  binned_file_path <- shinyFiles::parseFilePaths(c(wd= rv$binned_base_dir),
                                             input$DS___p___binned_data)
  req(binned_file_path$datapath) # Do not remove
  rv$binned_file_name <- binned_file_path$datapath

  # Create binned_data
  load(rv$binned_file_name)
  rv$binned_data <- binned_data

  # Create list of variables from binned_data labels
  rv$binned_labels <- sub("labels.", "",
                           names(select(binned_data, starts_with("labels"))))
})


# Show binned data path if selected
output$DS_show_chosen_bin <- renderText({
  if(is.na(rv$binned_file_name)){
    "No file chosen yet"
  }else{
    basename(rv$binned_file_name)
  }
})

################################################################################
################################ Basic outputs #################################
################################################################################

# List of labels to decode
output$DS_basic___p___list_of_labels <- renderUI({
  req(rv$binned_file_name)
  selectInput("DS_basic___p___labels",
              "Variable to decode and to use",
              rv$binned_labels)
})

# Find all levels to use in the data
reactive_all_basic_levels_to_use  <- reactive({
  req(rv$binned_file_name)
  binned_data <- rv$binned_data
  levels(factor(binned_data[[paste0("labels.",input$DS_basic___p___labels)]]))
})

# Use reactive_all_basic_levels_to_use to set options in drop down
output$DS_basic___np___list_of_levels_to_use <- renderUI({
  selectInput("DS_basic___p___label_levels",
              "Levels to use",
              reactive_all_basic_levels_to_use(),
              multiple = TRUE)
})

# Drop down to count data
output$DS_basic___p___use_count_data <- renderUI({
  selectInput("DS_basic___p___use_count_data",
              "Convert the data into spike counts",
              c(FALSE, TRUE))
})

# Find the available site ids to select
reactive_all_basic_site_IDs_to_use <- reactive({
  req(rv$binned_file_name)
  binned_data <- rv$binned_data
  levels(unique(factor(binned_data$siteID)))
})

# Use reactive_all_basic_site_IDs_to_use to select the ids
output$DS_basic___p___site_IDs_to_use <- renderUI({
  selectInput("DS_basic___p___site_IDs_to_use",
              "Which sites should be used",
              reactive_all_basic_site_IDs_to_use(),
              multiple = TRUE)
})

# Find the available site ids to exclude
reactive_all_basic_site_IDs_to_exclude <- reactive({
  req(rv$binned_file_name)
  binned_data <- rv$binned_data
  site_IDs <- levels(unique(factor(binned_data$siteID)))
  # If no site ids were selected, then offer all
  if (is.null(input$DS_basic___p___site_IDs_to_use)){
    site_IDs
  # If some were selected, remove them from the options
  }else{
    site_IDs[-(which(site_IDs %in% input$DS_basic___p___site_IDs_to_use))]
  }

})

# Use reactive_all_basic_site_IDs_to_exclude to select the ids
output$DS_basic___p___site_IDs_to_exclude <- renderUI({
  selectInput("DS_basic___p___site_IDs_to_exclude",
              "Which sites should be excluded",
              reactive_all_basic_site_IDs_to_exclude(),
              multiple = TRUE)
})


# Offer the options to randomly shuffle labels
output$DS_basic___p___randomly_shuffled_labels <- renderUI({
  selectInput("DS_basic___p___randomly_shuffled_labels",
              "Randomly shuffle labels",
              c(FALSE, TRUE))
})

# Offer the option to create simultaneous populations
output$DS_basic___p___create_simultaneous_populations <- renderUI({
  selectInput("DS_basic___p___create_simultaneous_populations",
              "Was the data created simultaneously?",
              c(0,1))
})

################################################################################
############################### General outputs ################################
################################################################################

# List of labels to decode
output$DS_gen___np___list_of_labels <- renderUI({
  req(rv$binned_file_name)
  selectInput("DS_gen___p___labels",
              "Variable to decode and to use",
              rv$binned_labels)
})


# DS_gen___np___class_number ----

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





