

# Elisa make it base direcotry
source("UIs/ui_binning_params.R")
source("UIs/ui_plot_raster.R")
source("UIs/ui_upload_new_raster.R")
source("UIs/ui_data_source.R")
source("UIs/ui_split_parameters.R")
source("UIs/ui_classifier.R")
source("UIs/ui_fp.R")
source("UIs/ui_rm.R")
source("UIs/ui_cv.R")
source("UIs/ui_run_analysis.R")
source("UIs/ui_view_results.R")
source("UIs/ui_plot_decoding.R")


sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Binning the raster data", tabName = "bin"),
              menuItem("Population decoding", tabName = "decode")
  )
)

decoding_analysis <- tabPanel(
  title = "Run a decoding analysis",
  fluidPage(
    fluidRow(
      column(width = 12,
             tabBox(width = 12,
                    data_source_tab, # selecting data and ds parameters
                    split_parameters_tab, # additional ds parameter
                    classifier_tab, # classifiers
                    fp_tab, # feature processors
                    rm_tab, # result metrics
                    cv_tab, # cross validator
                    run_analysis_tab, # running and generating scripts
                    view_results_tab # viewing pdf output of the script
             )
      )
    )
  )
)


# Combining sidebar bodies ----
body <- dashboardBody(tabItems(tabItem(tabName = "bin",
                                       navbarPage(title = "",
                                                  binning_params,
                                                  plot_raster,
                                                  upload_raster)),
                               tabItem(tabName = "decode",
                                       navbarPage(title = "",
                                                  decoding_analysis,
                                                  plot_decoding)
                               )
)
)



myUI <- shinyUI({
  dashboardPage(skin = "green",
                dashboardHeader(title = "NeuroShiny"),
                sidebar,
                body)
})







