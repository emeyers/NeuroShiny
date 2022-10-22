
all_result_type <- c("zero_one_loss", "normalized_rank", "decision_values", "all")

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
                tabPanel("TCT heatmap",
                         selectInput("plot_tct_result_type",
                                     NULL,
                                     all_result_type),
                         plotOutput("plot_tct")
                ),
                tabPanel("PDF of script and result",
                         actionButton("plot_create_pdf", "Create"),
                         helpText(""),
                         uiOutput("plot_pdf") #Elisa - this doesn't work
                )
         )
  )
)
