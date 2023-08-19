
################################################################################
########################## Cross validator parameters ##########################
################################################################################

# Number of resample runs
output$CV_standard___p___num_resample_runs <- renderUI({
  numericInput("CV_standard___p___num_resample_runs",
               "Number of resampling runs",
               value = 50, min = 1)

})

# Creating parallel cores for resample runs
output$CV_standard___p___num_parallel_cores <- renderUI({
  numericInput("CV_standard___p___num_parallel_cores",
               "Number of parallel cores for resample runs",
               value = NULL)
})

# Creating a parallel outfile
output$CV_standard___p___parallel_outfile <- renderUI({
  # These requirements may change, check online for most recent requirements
  req(!is.null(input$CV_standard___p___num_parallel_cores))
  req(input$CV_standard___p___num_parallel_cores >= 1)

  # Allow for a written input for outfile if requirements met
  textInput("CV_standard___p___parallel_outfile",
            "File name of output from parallel cores",
            value = "")
})


