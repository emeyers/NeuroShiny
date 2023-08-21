
# Tab to output most recent results as a pdf
current_results_tab <-
  tabPanel(
    title = "Current Results",
    width = NULL,
    solidHeader = TRUE, status = "primary",
    uiOutput("DC_plot_pdf")
  )
