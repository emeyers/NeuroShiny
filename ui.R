

# Elisa make it base directory project tab in the binning spot instead and force
# them to put in the location of the base directory

source("UIs/ui_select_project.R")
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
source("UIs/ui_current_results.R")
source("UIs/ui_plot_single_result.R")
source("UIs/ui_plot_multi_result.R")


sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Select Your Project",
                       icon = icon("folder-open"),
                       tabName = "project"),
              menuItemOutput("menuBin"),
              menuItemOutput("menuDecode")
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
                    current_results_tab # view pdf
                    )
             )
      )
    )
  )


# Combining sidebar bodies ----
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "project", select_proj_tab),
    tabItem(tabName = "bin",
            navbarPage(title = "",
                       binning_params,
                       plot_raster,
                       upload_raster)),
    tabItem(tabName = "decode",
            navbarPage(title = "",
                       decoding_analysis,
                       plot_decoding,
                       plot_manifest)
    )
  )
)



myUI <- shinyUI({
  dashboardPage(skin = "green",
                dashboardHeader(title = "NeuroShiny"),
                sidebar,
                body)
})







