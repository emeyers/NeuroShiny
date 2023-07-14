
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
                 helpText("We only accept .mat and .Rda format"),
                 shinyFiles::shinyDirButton("bin_chosen_raster", "Browse", ""),
                 helpText("Loaded raster data: "),
                 textOutput("bin_show_chosen_raster")),
             shinydashboard::box(width = 8,
                 numericInput("bin_bin_width", "Bin width", value = 10, min = 1),
                 numericInput("bin_step_size", "Step size", value = 1, min = 1),
                 numericInput("bin_start_ind",
                              "Index of the sample where the first bin starts (optional)",
                              value = NULL),
                 numericInput("bin_end_ind",
                              "Index of the sample where the last bin ends (optional)",
                              value = NULL),
                 textInput("bin_prefix_of_binned_file_name",
                           "prefix of binned file name (e.g., ZD)"),
                 actionButton("bin_the_data", "Bin the data"),
                 textOutput("bin_show_create_bin_function_run")
             )
      )
    )
  )
)

