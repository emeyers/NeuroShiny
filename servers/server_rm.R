
################################################################################
################################ Result metrics ################################
################################################################################

# Select the type of result metric from hard coded list in global
output$RM_type <- renderUI({
  checkboxGroupInput("RM_type",
                     "Result Metrics",
                     all_rm,
                     selected = all_rm)
})

################################################################################
############################## Main result options #############################
################################################################################

# Header text to denote the additional checkbox(es)
output$RM_mr___np___include_norm_rank_results_text <- renderText({
  if("rm_main_results" %in% input$RM_type){
    "<br><strong>Parameters for rm_main_results</strong>"
  }
})

# Option to include normalized rank results
output$RM_mr___p___include_norm_rank_results <- renderUI({
  if("rm_main_results" %in% input$RM_type){
    checkboxInput("RM_mr___p___include_norm_rank_results",
                  "Include normalized rank results", value = TRUE)
  }
})

################################################################################
########################### Confusion matrix options ###########################
################################################################################

# Header text to denote the additional checkbox(es)
output$RM_cm___np___text <- renderText({
  if("rm_confusion_matrix" %in% input$RM_type){
    "<br><strong> Parameters for rm_confusion_matrix </strong>"
  }
})

# Option to save TCD results
output$RM_cm___p___save_TCD_results <- renderUI({
  if("rm_confusion_matrix" %in% input$RM_type){
    checkboxInput("RM_cm___p___save_TCD_results",
                  "Save results only for training and testing at the same time",
                  value = TRUE, width = '100%')
  }
})

# Option to create decision value confusion matrix
output$RM_cm___p___create_decision_vals_confusion_matrix <- renderUI({
  if("rm_confusion_matrix" %in% input$RM_type){
    checkboxInput("RM_cm___p___create_decision_vals_confusion_matrix",
                  "Create decision value confusion matrix", value = TRUE)
  }
})
