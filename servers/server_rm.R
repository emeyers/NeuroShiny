
all_rm <- c("rm_main_results", "rm_confusion_matrix")

# RM_type ----
output$RM_type = renderUI({
  checkboxGroupInput("RM_type",
                     "Result Metrics",
                     all_rm,
                     selected = all_rm)
})


# RM_mr___np___include_norm_rank_results_text ----
output$RM_mr___np___include_norm_rank_results_text = renderText({
  if("rm_main_results" %in% input$RM_type){
    "<br>Parameters for rm_main_results:"
  }
})


# RM_mr___p___include_norm_rank_results ----
output$RM_mr___p___include_norm_rank_results = renderUI({
  #req(input$RM_mr___p___include_norm_rank_results)
  if("rm_main_results" %in% input$RM_type){
    checkboxInput("RM_mr___p___include_norm_rank_results",
                  " Include normalized rank results", value = TRUE)
  }
})


# RM_cm___np___text ----
output$RM_cm___np___text = renderText({
  if("rm_confusion_matrix" %in% input$RM_type){
    "<br>Parameters for rm_confusion_matrix:"
  }
})


# RM_cm___p___save_TCD_results ----
output$RM_cm___p___save_TCD_results = renderUI({
  #req(input$RM_mr___p___include_norm_rank_results)
  if("rm_confusion_matrix" %in% input$RM_type){
    checkboxInput("RM_cm___p___save_TCD_results",
                  " Save results only for training and testing at the same time",
                  value = TRUE, width = '100%')
  }
})


# RM_cm___p___create_decision_vals_confusion_matrix ----
output$RM_cm___p___create_decision_vals_confusion_matrix = renderUI({
  #req(input$RM_mr___p___include_norm_rank_results)
  if("rm_confusion_matrix" %in% input$RM_type){
    checkboxInput("RM_cm___p___create_decision_vals_confusion_matrix",
                  " Create decision value confusion matrix", value = TRUE)
  }
})


