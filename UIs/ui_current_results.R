
# Tab to output most recent results as a pdf
current_results_tab <-
  tabPanel(
    title = "Current Results",
    value = 1,
    width = NULL,
    solidHeader = TRUE, status = "primary",
    uiOutput("DC_plot_pdf")
  )
