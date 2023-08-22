
# Binning parameters with input parameters and selected raster
binning_params <- tabPanel(
  title = "Specify binning parameters",
  fluidPage(
    fluidRow(
      column(width = 12,
             shinydashboard::box(width = NULL,
                 status = "danger",
                 solidHeader = TRUE,
                 title = "Choose a directory of raster data files",
                 helpText("We only accept .mat, .Rda, .csv formats"),
                 uiOutput("bin_chosen_raster"),
                 helpText("Loaded raster data: "),
                 textOutput("bin_show_chosen_raster")),
             shinydashboard::box(width = 8,
                 html2Text("Required Parameters:"),
                 numericInput("bin_bin_width", "Bin width", value = 10, min = 1),
                 numericInput("bin_step_size", "Step size", value = 1, min = 1),
                 textInput("bin_file_name_prefix",
                           "Prefix of binned file name (e.g., project abbr.)"),
                 html2Text("Optional Parameters:"),
                 numericInput("bin_start_time",
                              "Index of the sample where the first bin starts",
                              value = NULL),
                 numericInput("bin_end_time",
                              "Index of the sample where the last bin ends",
                              value = NULL),
                 actionButton("bin_the_data", "Bin the data"),
                 html2Text("The following code was most recently run:"),
                 htmlOutput("show_binning_function")
             )
      )
    )
  )
)

