
# Tab for plotting single results under the population decoding
plot_decoding <- tabPanel(
  title = "Plot single result",
  column(width = 12,
         box(
           title = "Choose the result to plot",
           width = NULL,
           status = "danger",
           solidHeader = TRUE,
           shinyFiles::shinyFilesButton("plot_chosen_result",
                                        "Browse", "", multiple = FALSE),
           helpText("Loaded result: "),
           textOutput("plot_show_chosen_result")),
         tabBox(width = NULL,
                id = "plot_decoding_tabs",
                tabPanel("Timeseries",
                         selectInput("plot_timeseries_result_type",
                                     "Type of result to plot",
                                     all_result_type),
                         withSpinner(plotOutput("plot_timeseries"),
                                     color = "#79c9da")),
                tabPanel("TCD heatmap",
                         selectInput("plot_tcd_result_type",
                                     "Type of results to plot",
                                     all_result_type),
                         withSpinner(plotOutput("plot_tcd"),
                                     color = "#79c9da")),
                tabPanel("Confusion Matrix",
                         selectInput("plot_cm_result_type",
                                     "Type of results to plot",
                                     cm_result_type),
                         withSpinner(plotOutput("plot_cm"),
                                     color = "#79c9da")),
                tabPanel("PDF of script and result",
                         actionButton("plot_create_pdf", "Create"),
                         helpText(""),
                         uiOutput("plot_pdf")) #Elisa - this doesn't work
         )
  )
)
