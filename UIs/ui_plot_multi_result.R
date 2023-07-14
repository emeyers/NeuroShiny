
# Plotting multiple results using the manifest file
plot_manifest <- tabPanel(
  title = "Plot multiple results",
  column(width = 12,
         box(
           title = "Choose your manifest file",
           width = NULL,
           status = "danger",
           solidHeader = TRUE,
           shinyFiles::shinyFilesButton("manifest_data",
                                        "Browse", "",
                                        multiple = FALSE),
           helpText("Current File: "),
           textOutput("show_chosen_manifest")),
         fluidRow(
           column(
             width = 4,
             selectInput("plot_multi_result_to_show",
                         "Results to show",
                         all_result_type),
             uiOutput("type_dropdown"),
             selectInput("legend_label_selection",
                         "Legend titles to use",
                         c("Analysis ID", "Result Name")),),
           column(
             width = 8,
             withSpinner(plotOutput("manifest_plot"),
                         color = "#79c9da")),
           DTOutput("manifest_table"))
  )
)

