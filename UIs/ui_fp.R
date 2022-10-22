

fp_tab <-
  tabPanel(
  title = "Feature Preprocessors",
  width = NULL,
  solidHeader = TRUE, status = "primary",
  #Select feature processors (multi select)
  uiOutput("FP_type"),
  #Select additional parameters if user chooses fp_select_k_features
  conditionalPanel(condition = "'fp_select_k_features' %in% input.FP_type",
                   uiOutput("FP_skf___p___num_sites_to_use"),
                   uiOutput("FP_skf___p___num_sites_to_exclude"))
)
