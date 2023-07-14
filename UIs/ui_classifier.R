
# Tab to select classifiers
# Note that there is no server file for this tab
classifier_tab <-
  tabPanel(
    title = "Classifier",
    width = NULL,
    solidHeader = TRUE, status = "primary",
    # Select between three classifier types (all_cl located in global.R)
    selectInput("CL_type", "Classifier", all_cl),
    box(
      width = NULL,
      title = "Additional parameters",
      checkboxInput("CL___p___return_decision_values",
                    "Return decision values?", TRUE),
      # Select additional parameters if classifier is SVM
      conditionalPanel(condition  = "input.CL_type == 'cl_svm'",
                       selectInput("CL_svm___p___kernel",
                                   "Kernel",
                                   c("linear", "polynomial", "radial", "sigmoid"),
                                   selected = "linear"),
                       numericInput("CL_svm___p___cost",
                                    "Cost",
                                    value = 1, min = 0),
                       # If polynomial kernel selected for SVM classifier:
                       conditionalPanel(condition ="input.CL_svm___p___kernel == 'polynomial'",
                                        numericInput("CL_svm___p___degree",
                                                     "Degree of polynomial",
                                                     value = 3,
                                                     min = 2,
                                                     max  = 10)),
                       # If radial/polynomial kernel selected for SVM classifier:
                       conditionalPanel(condition = "input.CL_svm___p___kernel == 'radial'|input.CL_svm___p___kernel == 'polynomial'",
                                        numericInput("CL_svm___p___coef0",
                                                     "Coef0",
                                                     0)),
                       # If radial/polynomial kernel selected for SVM classifier:
                       conditionalPanel(condition = "input.CL_svm___p___kernel == 'radial'|input.CL_svm___p___kernel == 'polynomial'|input.CL_svm___p___kernel == 'sigmoid'",
                                        numericInput("CL_svm___p___gamma",
                                                     "Gamma",
                                                     NULL))
      )
    )
  )
