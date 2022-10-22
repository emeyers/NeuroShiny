
rm_tab <-
  tabPanel(
    title = "Result Metrics",
    width = NULL,
    solidHeader = TRUE, status = "primary",
    #Select result metrics type (multi select)
    uiOutput("RM_type"),
    #Select additional parameters if user chooses rm_main_results
    conditionalPanel(condition = "'rm_main_results' %in% input.RM_type",
                     htmlOutput("RM_mr___np___include_norm_rank_results_text"),
                     uiOutput("RM_mr___p___include_norm_rank_results")),
    #Select additional parameters if user chooses RM_cm___np___text
    conditionalPanel(condition = "'rm_confusion_matrix' %in% input.RM_type",
                     htmlOutput("RM_cm___np___text"),
                     uiOutput("RM_cm___p___save_TCD_results"),
                     uiOutput("RM_cm___p___create_decision_vals_confusion_matrix"))
  )

