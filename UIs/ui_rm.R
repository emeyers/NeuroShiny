
# Tab to select result metrics
rm_tab <-
  tabPanel(
    title = "Result Metrics",
    width = NULL,
    solidHeader = TRUE, status = "primary",
    # Select result metrics type (multi select)
    uiOutput("RM_type"),
    # Select additional parameters if user chooses rm_main_results
    # Conditionally shown with code from server, conditional panel doesn't work
    htmlOutput("RM_mr___np___include_norm_rank_results_text"),
    uiOutput("RM_mr___p___include_norm_rank_results"),
    # Select additional parameters if user chooses rm_confusion_matrix
    # Conditionally shown with code from server, conditional panel doesn't work
    htmlOutput("RM_cm___np___text"),
    uiOutput("RM_cm___p___save_TCD_results"),
    uiOutput("RM_cm___p___create_decision_vals_confusion_matrix")
  )

