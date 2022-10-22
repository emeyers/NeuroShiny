

view_results_tab <-
  tabPanel(
    title = "View Results",
    width = NULL,
    solidHeader = TRUE, status = "primary",
    uiOutput("DC_plot_pdf")
  )
