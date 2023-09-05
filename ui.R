
################################################################################
############################## UI source files #################################
################################################################################

# UI files for project selection sidebar
source("UIs/ui_select_project.R")
source("UIs/ui_github.R")

# UI files for binning tabs
source("UIs/ui_binning_params.R")
source("UIs/ui_plot_raster.R")
source("UIs/ui_upload_new_raster.R")

# UI files for decoding tabs
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


################################################################################
############################### Create sidebar #################################
################################################################################

sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Select Your Project",
                       icon = icon("folder-open"),
                       tabName = "project"),
              menuItemOutput("menu_bin"),
              menuItemOutput("menu_decode")
  )
)

################################################################################
########################### Sub-tabs for decoding ##############################
################################################################################

decoding_analysis <- tabPanel(
  title = "Run a decoding analysis",
  fluidPage(
    fluidRow(
      column(width = 12,
             tabBox(width = 12,
                    id = "decoding_tabs",
                    data_source_tab, # Selecting data and ds parameters
                    split_parameters_tab, # Additional ds parameter
                    classifier_tab, # Classifiers
                    fp_tab, # Feature processors
                    rm_tab, # Result metrics
                    cv_tab, # Cross validator
                    run_analysis_tab, # Running and generating scripts
                    current_results_tab # View pdf
                    )
             )
      )
    )
  )

################################################################################
########################### Combining page bodies ##############################
################################################################################

body <- dashboardBody(
  tabItems(
    # Tab to open project folder
    tabItem(tabName = "project",
            navbarPage(title = "",
                       select_proj_tab,
                       github_tab)),
    # Tab for binning the data
    tabItem(tabName = "bin",
            navbarPage(title = "",
                       binning_params, # Binning from selected file
                       plot_raster,
                       upload_new_raster)),
    # Tab for neuro-decoding
    tabItem(tabName = "decode",
            navbarPage(title = "",
                       decoding_analysis, # Decoding with sub-tabs
                       plot_decoding, # Plotting single result
                       plot_manifest) # Plotting multiple result with manifest
    )
  )
)

################################################################################
####################### Combining sidebars and bodies ##########################
################################################################################

# This is the UI object passed into the app.R file to creating the app
myUI <- shinyUI({
  dashboardPage(skin = "green",
                dashboardHeader(title = "NeuroShiny"),
                sidebar,
                body)
})
