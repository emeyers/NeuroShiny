
################################################################################
################################## Render PDF ##################################
################################################################################

output$DC_plot_pdf <- renderUI({
  # Render the pdf depending on knitting status
  if (is.null(rv$pdf_knitting_status)) {
    return("No R results have been knit yet")
  } else if (rv$pdf_knitting_status == "running") {
    # This is never run b/c when code is knitting it holds up any updates to the UI :(
    # I think I will need to use the promise package to knit asynchronously to get this to work :/
    return("R results are in the process of being knit a pdf...")
  } else if (rv$pdf_knitting_status == "error") {
    # If the code crashed and could not be knit
    return("R results could not be knit due to error in script")
  } else {
    tags$iframe(style="height:600px; width:100%", src = rv$latest_pdf_file_name)
  }
})
