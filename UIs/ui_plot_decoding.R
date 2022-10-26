

plot_decoding <- tabPanel(
  title = "Plot decoding results",
  column(width = 12,
         box(
           title = "Choose the result to plot",
           width = NULL,
           status = "danger",
           solidHeader = TRUE,
           shinyFiles::shinyFilesButton("plot_chosen_result",
                                        "Browse", "", multiple = FALSE),
           helpText("Loaded result: "),
           textOutput("plot_show_chosen_result")
         ),
         tabBox(width = NULL,
                tabPanel("Timeseries",
                         selectInput("plot_timeseries_result_type",
                                     "Type of result to plot",
                                     all_result_type),
                         plotOutput("plot_timeseries")
                ),
                tabPanel("TCD heatmap",
                         selectInput("plot_tcd_result_type",
                                     "Type of results to plot",
                                     all_result_type),
                         plotOutput("plot_tcd")
                ),
                tabPanel("Confusion Matrix",
                         selectInput("plot_cm_result_type",
                                     "Type of results to plot",
                                     cm_result_type),
                         plotOutput("plot_cm")
                ),
                tabPanel("PDF of script and result",
                         actionButton("plot_create_pdf", "Create"),
                         helpText(""),
                         uiOutput("plot_pdf") #Elisa - this doesn't work
                )
         )
  )
)
