

# CV_standard___p___num_resample_runs ----
output$CV_standard___p___num_resample_runs = renderUI({
  numericInput("CV_standard___p___num_resample_runs",
               "Number of resampling runs",
               value = 50, min = 1)

})


# CV_standard___p___num_parallel_cores ----
output$CV_standard___p___num_parallel_cores = renderUI({
  numericInput("CV_standard___p___num_parallel_cores",
               "Number of parallel cores for resample runs",
               value = NULL)
})


# CV_standard___p___parallel_outfile ----
output$CV_standard___p___parallel_outfile = renderUI({
  req(!is.null(input$CV_standard___p___num_parallel_cores))
  req(input$CV_standard___p___num_parallel_cores >= 1)

  textInput("CV_standard___p___parallel_outfile",
            "File name of output from parallel cores",
            value = NULL)
})



