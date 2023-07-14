
################################################################################
################################## Render PDF ##################################
################################################################################
output$DC_plot_pdf <- renderUI({
  # Elisa, missing the two rvs
  if (is.null(rv$pdf_knitting_status)) {
    return("No R Markdown results have been knit yet")
  } else if (rv$pdf_knitting_status == "running") {
    # This is never run b/c when code is knitting it holds up any updates to the UI :(
    # I think I will need to use the promise package to knit asynchronously to get this to work :/
    return("R Markdown results are in the process of being knit a pdf...")
  } else {
    tags$iframe(style="height:600px; width:100%", src = rv$latest_pdf_file_name)
  }
})
