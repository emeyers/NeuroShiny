
all_cl <- c("cl_max_correlation", "cl_svm", "cl_poisson_naive_bayes")
all_fp <- c("fp_zscore", "fp_select_k_features")
cl_fp <- data.frame(c(1, 1), c(1, 1), c(1, 0))
colnames(cl_fp) <- all_cl
rownames(cl_fp) <- all_fp

# FP_type ----
output$FP_type = renderUI({
  checkboxGroupInput("FP_type",
                     "Feature Preprocessors",
                     reactive_all_fp_avail(),
                     selected = "fp_zscore")
})

reactive_all_fp_avail <- reactive({
  req(input$CL_type)
  all_fp[cl_fp[,input$CL_type]>0]
})


# FP_skf___p___num_sites_to_use ----
output$FP_skf___p___num_sites_to_use = renderUI({
  req(input$FP_type)
  if("fp_select_k_features" %in% input$FP_type){
    numericInput("FP_skf___p___num_sites_to_use",
                 "Select top features? (this will be applied first)",
                 reactive_bin_num_neuron(),
                 min = 1,
                 max = reactive_bin_num_neuron())
  }
})

reactive_bin_num_neuron <- reactive({
  validate(need(input$DS___p___binned_data,
                "Please select data source first to get total number of neurons"))
  binned_data = rv$binned_data
  length(unique(factor(binned_data$siteID)))
})


# FP_skf___p___num_sites_to_exclude ----
output$FP_skf___p___num_sites_to_exclude = renderUI({
  req(input$FP_skf___p___num_sites_to_use)
  if("fp_select_k_features" %in% input$FP_type){
    numericInput("FP_skf___p___num_sites_to_exclude",
                 "exclude top ? features (this will be applied second)",
                 value = 0,
                 min = 0,
                 max = reactive_bin_num_neuron() - input$FP_skf___p___num_sites_to_use)
  }
})
