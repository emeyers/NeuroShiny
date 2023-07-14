
# Tab to select feature preprocessors, requires selection of classifiers
fp_tab <-
  tabPanel(
  title = "Feature Preprocessors",
  width = NULL,
  solidHeader = TRUE, status = "primary",
  # Select feature processors
  uiOutput("FP_type"),
  # Select additional parameters if user chooses fp_select_k_features
  # Condition set in server, conditionalPanel does not work with %in%
  uiOutput("FP_skf___p___num_sites_to_use"),
  uiOutput("FP_skf___p___num_sites_to_exclude")
)
