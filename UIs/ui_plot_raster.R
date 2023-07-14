
# Plot raster data in binning subtab after uploading it
plot_raster <- tabPanel(
  title = "Plot raster data",
  fluidPage(
    fluidRow(
      column(width = 12,
             box(width = NULL,
                 actionButton("bin_pre_neuron", "previous file"),
                 actionButton("bin_next_neuron", "next file"),
                 textOutput("bin_show_raster_cur_file_name")),
             tabBox(width = NULL,
                    title = "",
                    tabPanel(
                      title = "Raster plot",
                      ribbon = TRUE,
                      title_side = "top right",
                      withSpinner(plotOutput("bin_raster_plot"),
                                  color = "#79c9da")),
                    tabPanel(
                      width = NULL,
                      title = "PSTH (Peristimulus time histogram)",
                      ribbon = TRUE,
                      title_side = "top right",
                      withSpinner(plotOutput("bin_PSTH"),
                                  color = "#79c9da"))
             )
      )
    )
  )
)
