
################################################################################
######################## Selecting feature preprocessors #######################
################################################################################

# Multi select for the feature preprocessors
output$FP_type <- renderUI({
  checkboxGroupInput("FP_type",
                     "Feature Preprocessors",
                     reactive_all_fp_avail(),
                     selected = "fp_zscore")
})

# Feature preprocessors available depending on classifiers selected
# Matrix used to compute options in the global file
reactive_all_fp_avail <- reactive({
  req(input$CL_type)
  all_fp[cl_fp[,input$CL_type] > 0]
})

################################################################################
########################### Select k features options ##########################
################################################################################

# Finding the number of total unique site ID's
reactive_bin_num_neuron <- reactive({
  validate(need(input$DS___p___binned_data,
                "Please select data source first to get total number of neurons"))
  binned_data <- rv$binned_data
  length(unique(factor(binned_data$siteID)))
})

# Option to select the top features
output$FP_skf___p___num_sites_to_use <- renderUI({
  req(input$FP_type)
  if("fp_select_k_features" %in% input$FP_type){
    numericInput("FP_skf___p___num_sites_to_use",
                 "Select top features? (this will be applied first)",
                 reactive_bin_num_neuron(),
                 min = 1,
                 max = reactive_bin_num_neuron())
  }
})

# Option to select number of sites to exclude
output$FP_skf___p___num_sites_to_exclude <- renderUI({
  req(input$FP_skf___p___num_sites_to_use)
  if("fp_select_k_features" %in% input$FP_type){
    numericInput("FP_skf___p___num_sites_to_exclude",
                 "exclude top ? features (this will be applied second)",
                 value = 0,
                 min = 0,
                 max = reactive_bin_num_neuron() - input$FP_skf___p___num_sites_to_use)
  }
})
