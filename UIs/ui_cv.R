

cv_tab <-
  tabPanel(
    title = "Cross Validator",
    width = NULL,
    solidHeader = TRUE, status = "primary",
    #Select cross validator parameters
    checkboxInput("CV_standard___p___run_TCD", "Test only at training times?",FALSE),
    uiOutput("CV_standard___p___num_resample_runs"),
    uiOutput("CV_standard___p___num_parallel_cores"),
    #Include parallel outfile parameter if conditions met (as defined by online reference)
    conditionalPanel(condition = "input.CV_standard___p___num_parallel_cores >= 1 | !input.CV_standard___p___num_parallel_cores",
                     uiOutput("CV_standard___p___parallel_outfile"))
  )
