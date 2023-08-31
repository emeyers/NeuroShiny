

################################################################################
######################### Reset value for project change #######################
################################################################################

observeEvent(rv$binned_base_dir,{
  # Reset the dropdown values to NULL when project is updated
  updateSelectInput(session, "DS___p___binned_data", choices = NULL)
})


################################################################################
######################### Select and load binned data ##########################
################################################################################

# Drop down to load binned data
observeEvent(rv$binned_base_dir,{
  rv$binned_data_to_decode <- list.files(rv$binned_base_dir, full.names = FALSE,
                                         pattern = "\\.Rda$")
})
output$DS___p___binned_data <- renderUI({
  selectInput("DS___p___binned_data", "", rv$binned_data_to_decode)
})


# If button has been clicked:
# Then load in data to find variable labels
observeEvent(input$DS___p___binned_data,{
  req(input$DS___p___binned_data, rv$binned_base_dir)

  # Create file path and set the reactive variable
  rv$binned_file_name <- file.path(rv$binned_base_dir, input$DS___p___binned_data)

  # Load binned_data and set its reactive variable
  load(rv$binned_file_name)
  rv$binned_data <- binned_data

  # Create list of variables from binned_data labels and set reactive variables
  rv$binned_labels <- sub("labels.", "",
                           names(select(binned_data, starts_with("labels"))))
})

# If bin selected
# Then how binned data path in ui
output$DS_show_chosen_bin <- renderText({
  if(is.na(rv$binned_file_name)){
    "No file chosen yet"
  } else {
    base_char <- gsub(app_base_dir, "", rv$binned_base_dir)
    file.path(base_char, basename(rv$binned_file_name))
  }
})

################################################################################
########################## Basic outputs - Standard ############################
################################################################################

# Input list of labels to decode
output$DS_basic___p___list_of_labels <- renderUI({
  req(rv$binned_file_name)
  selectInput("DS_basic___p___labels",
              "Variable to decode and to use",
              rv$binned_labels)
})

# Find all levels to use in the data
reactive_all_basic_levels_to_use  <- reactive({
  req(rv$binned_file_name)
  levels(factor(rv$binned_data[[paste0("labels.", input$DS_basic___p___labels)]]))
})

# Input drop down using reactive_all_basic_levels_to_use as options
output$DS_basic___np___list_of_levels_to_use <- renderUI({
  selectInput("DS_basic___p___label_levels",
              "Levels to use",
              reactive_all_basic_levels_to_use(),
              multiple = TRUE)
})

################################################################################
########################## Basic outputs - Advanced ############################
################################################################################

# Input drop down for count data variable
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

# Input for selected ids using reactive_all_basic_site_IDs_to_use options
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
  } else {
    site_IDs[-(which(site_IDs %in% input$DS_basic___p___site_IDs_to_use))]
  }

})

# Input for selected ids using reactive_all_basic_site_IDs_to_exclude options
output$DS_basic___p___site_IDs_to_exclude <- renderUI({
  selectInput("DS_basic___p___site_IDs_to_exclude",
              "Which sites should be excluded",
              reactive_all_basic_site_IDs_to_exclude(),
              multiple = TRUE)
})

# Input to offer randomly shuffle labels
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
######################### General outputs - Standard ###########################
################################################################################

# List of labels to decode
output$DS_gen___np___list_of_labels <- renderUI({
  req(rv$binned_file_name)
  selectInput("DS_gen___p___labels",
              "Variable to decode and to use",
              rv$binned_labels)
})

# Given a change in input$DS_gen___np___class_number (number of classes)
# Append new value to previous values
observeEvent(input$DS_gen___np___class_number, {
  rv$prev_bins <- c(tail(rv$prev_bins, 1), input$DS_gen___np___class_number)
})

# Reactive function to find the levels
reactive_all_levels_of_gen_var_to_use <- reactive({
  req(rv$binned_file_name)
  binned_data <- rv$binned_data
  levels(factor(binned_data[[paste0("labels.", input$DS_gen___p___labels)]]))
})

# Input for levels for each class using reactive_all_levels_of_gen_var_to_use()
output$DS_gen___p___label_levels <- renderUI({
  req(rv$binned_file_name)
  req(input$DS_gen___np___class_number)

  # Create empty list to append the rows
  all_rows <- list()

  # For the number of classes in each train and test,
  # Add selected levels using reactive_all_levels_of_gen_var_to_use() options
  for (i in 1:input$DS_gen___np___class_number){
    current_row <- fluidRow(
      # Train input
      column(width = 5,
             selectInput(paste0("DS_gen___p___train_label_levels_class_", i),
                         paste("Class", i, "- Training levels to use"),
                         reactive_all_levels_of_gen_var_to_use(),
                         multiple = TRUE)
      ),
      # Test input
      column(width = 5,
             selectInput(paste0("DS_gen___p___test_label_levels_class_", i),
                         paste("Class", i, "- Testing levels to use"),
                         reactive_all_levels_of_gen_var_to_use(),
                         multiple = TRUE)
      )
    )
    # Add current class row to list
    all_rows[[i]] <- current_row
  }
  # Output all the rows for each class
  all_rows

})

################################################################################
######################### General outputs - Advanced ###########################
################################################################################

# Input drop down for count data variable
output$DS_gen___p___use_count_data <- renderUI({
  selectInput("DS_gen___p___use_count_data",
              "Convert the data into spike counts",
              c(FALSE, TRUE))
})

# Find the available site ids to select
reactive_all_gen_site_IDs_to_use <- reactive({
  req(rv$binned_file_name)
  binned_data <- rv$binned_data
  levels(unique(factor(binned_data$siteID)))
})

# Input for selected ids using reactive_all_gen_site_IDs_to_use options
output$DS_gen___p___site_IDs_to_use <- renderUI({
  selectInput("DS_gen___p___site_IDs_to_use",
              "Which sites should be used",
              reactive_all_gen_site_IDs_to_use(),
              multiple = TRUE)
})

# Find the available site ids to exclude
reactive_all_gen_site_IDs_to_exclude <- reactive({
  req(rv$binned_file_name)
  binned_data <- rv$binned_data
  selected_levels <- levels(unique(factor(binned_data$siteID)))
  # If no site ids were selected, then offer all
  if (is.null(input$DS_gen___p___site_IDs_to_use)){
    selected_levels
  # If some were selected, remove them from the options
  } else {
    selected_levels[-(which(selected_levels %in% input$DS_gen___p___site_IDs_to_use))]
  }
})

# Input for selected ids using reactive_all_gen_site_IDs_to_exclude options
output$DS_gen___p___site_IDs_to_exclude <- renderUI({
  selectInput("DS_gen___p___site_IDs_to_exclude",
              "Which sites should be excluded",
              reactive_all_gen_site_IDs_to_exclude(),
              multiple = TRUE)
})

# Input to offer randomly shuffle labels
output$DS_gen___p___randomly_shuffled_labels <- renderUI({
  selectInput("DS_gen___p___randomly_shuffled_labels",
              "Randomly shuffle labels",
              c(FALSE, TRUE))
})

# Offer the option to create simultaneous populations
output$DS_gen___p___create_simultaneous_populations <- renderUI({
  selectInput("DS_basic___p___create_simultaneous_populations",
              "Was the data created simultaneously?",
              c(0,1))
})

