# Sidebar tabs----
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
              menuItem("Binning the raster data", tabName = "bin"),
              menuItem("Population decoding", tabName = "decode")
              )
)

## Binning the raster data ----
### Specify binning parameters ----
binning_params <- tabPanel(
  title = "Specify binning parameters",
  fluidPage(
    fluidRow(
      column(width = 12,
             box(width = NULL,
                 status = "danger",
                 solidHeader = TRUE,
                 title = "Choose a directory of raster data files",
                 shinyFiles::shinyDirButton("bin_chosen_raster", "Browse", ""),
                 helpText("Loaded raster data: "),
                 textOutput("bin_show_chosen_raster")
                 ),
             box(width = 8,
                 numericInput("bin_bin_width", "Bin width", value = 10, min = 1),
                 numericInput("bin_step_size", "Step size", value = 1, min = 1),
                 numericInput("bin_start_ind",
                              "Index of the sample where the first bin starts (optional)",
                              value = NULL),
                 numericInput("bin_end_ind",
                              "Index of the sample where the last bin ends (optional)",
                              value = NULL),
                 textInput("bin_prefix_of_binned_file_name", "prefix of binned file name (e.g., ZD)"),
                 actionButton("bin_bin_data", "Bin the data"),
                 uiOutput("bin_action_error"),
                 textOutput("bin_show_create_bin_function_run")
                 )
             )
      )
    )
  )

### Plot raster data ----
plot_raster <- tabPanel(
  title = "Plot raster data",
  fluidPage(
    fluidRow(
      column(width = 12,
             box(width = NULL,
                 actionButton("bin_pre_neuron", "previous file"),
                 actionButton("bin_next_neuron", "next file"),
                 textOutput("bin_show_raster_cur_file_name"),
                 dataTableOutput('where')
                 ),
             tabBox(width = NULL,
                    title = "",
                    tabPanel(
                      title = "Raster plot",
                      ribbon = TRUE,
                      title_side = "top right",
                      plotOutput("bin_raster_plot")
                      ),
                    tabPanel(
                      width = NULL,
                      title = "PSTH (Peristimulus time histogram)",
                      ribbon = TRUE,
                      title_side = "top right",
                      plotOutput("bin_PSTH")
                      )
                    )
             )
      )
    )
  )

### Upload new raster data ----
upload_raster <- tabPanel(
  title = "Upload new raster data",
  fluidRow(box(width = NULL,
               helpText("We only accept .mat and .Rda format !"),
               uiOutput("bin_offer_upload_raster"),
               uiOutput("bin_offer_create_raster"),
               uiOutput("bin_evil_raster"),
               textOutput("bin_show_create_raster_function_run")
               )
           )
  )


## Population decoding body ----
### Run a decoding analysis ----
decoding_analysis <- tabPanel(
  title = "Run a decoding analysis",
  fluidPage(
    fluidRow(
      column(width = 12,
             tabBox(width = 12,
                    #First decoding tab: selecting data file, data source (ds), and ds parameters
                    tabPanel(
                      title = "Data Source",
                      width = NULL,
                      solidHeader = TRUE,
                      status = "primary",
                      box(width = NULL,
                          title = "Choose a binned data file",
                          status = "danger",
                          solidHeader = TRUE,
                          shinyFiles::shinyFilesButton("DS___p___binned_data", "Browse", "", multiple = FALSE),
                          helpText("Loaded binned data: "),
                          textOutput("DS_show_chosen_bin")),
                      #Select DS
                      selectInput("DS_type", "Type of data source", c("ds_basic","ds_generalization")),
                      #DS Basic options
                      conditionalPanel(condition = "input.DS_type == 'ds_basic'",
                                       uiOutput("DS_basic___p___list_of_labels"),
                                       checkboxInput("DS_basic___np___select_levels", "Select your specific levels", FALSE),
                                       checkboxInput("DS_basic___np___advanced", "Select advanced parameters?", FALSE)
                      ),
                      #Option to specify levels for DS Basic
                      conditionalPanel(condition = "input.DS_basic___np___select_levels && input.DS_type == 'ds_basic'",
                                       uiOutput("DS_basic___np___list_of_levels_to_use")
                      ),
                      #Optional advanced parameters for DS Basic
                      conditionalPanel(condition = "input.DS_basic___np___advanced && input.DS_type == 'ds_basic'",
                                       uiOutput("DS_basic___p___use_count_data"),
                                       uiOutput("DS_basic___p___site_IDs_to_use"),
                                       uiOutput("DS_basic___p___site_IDs_to_exclude"),
                                       uiOutput("DS_basic___p___randomly_shuffled_labels"),
                                       uiOutput("DS_basic___p___create_simultaneous_populations")
                      ),
                      #DS Generalization options
                      conditionalPanel(condition = "input.DS_type == 'ds_generalization'",
                                       uiOutput("DS_gen___np___list_of_labels"),
                                       #uiOutput("DS_gen___np___select_num_of_groups"),
                                       numericInput("DS_gen___np___class_number","How many classes?", 2, min = 2),
                                       uiOutput("DS_gen___p___label_levels"),
                                       checkboxInput("DS_gen___np___advanced", "Select advanced parameters?", FALSE)
                      ),
                      #Optional advanced parameters for DS Generalization
                      conditionalPanel(condition = "input.DS_gen___np___advanced && input.DS_type == 'ds_generalization'",
                                       uiOutput("DS_gen___p___use_count_data"),
                                       uiOutput("DS_gen___p___site_IDs_to_use"),
                                       uiOutput("DS_gen___p___site_IDs_to_exclude"),
                                       uiOutput("DS_gen___p___randomly_shuffled_labels"),
                                       uiOutput("DS_gen___p___create_simultaneous_populations")
                      )
                    ),

                    #Second decoding tab: additional ds parameters based on output graph for repetition info
                    tabPanel(
                      title = "DS Split Parameters",
                      width = NULL,
                      solidHeader = TRUE, status = "primary",
                      fluidRow(
                        column(
                          width = 10,
                          #Output the max rep (text ouput for backend calculation)
                          uiOutput("DS_max_repetition_avail_with_any_site"),
                          #DS Basic parameters
                          conditionalPanel(condition = "input.DS_type == 'ds_basic'",
                                           uiOutput("DS_basic___p___num_cv_splits"),
                                           uiOutput("DS_basic___p___num_label_repeats_per_cv_split"),
                                           uiOutput("DS_show_chosen_repetition_info"),
                                           uiOutput("DS_basic___p___num_resample_sites")
                          ),
                          #DS Generalization parameters
                          conditionalPanel(condition = "input.DS_type == 'ds_generalization'",
                                           uiOutput("DS_gen___p___num_cv_splits"),
                                           uiOutput("DS_gen___p___num_label_repeats_per_cv_split"),
                                           #uiOutput("DS_show_chosen_repetition_info"), ELISA
                                           uiOutput("DS_gen___p___num_resample_sites")
                          ),
                          #plot output for number of sites against number of repeated conditions
                          plotlyOutput("DS_show_level_repetition_info")
                        )
                      )
                    ),

                    #Third decoding tab: classifiers
                    tabPanel(
                      title = "Classifier",
                      width = NULL,
                      solidHeader = TRUE, status = "primary",
                      #Select between three classifier types (all_cl located in global.R)
                      selectInput("CL_type", "Classifier", all_cl),
                      box(
                        width = NULL,
                        title = "Additional parameters (if applicable)",
                        #Select parameters if classifier is svm
                        conditionalPanel(condition  = "input.CL_type == 'cl_svm'",
                                         selectInput("CL_svm___p___kernel",
                                                     "Kernel",
                                                     c("linear", "polynomial", "radial", "sigmoid"),
                                                     selected = "linear"),
                                         numericInput("CL_svm___p___cost",
                                                      "Cost",
                                                      value = 1, min = 0),
                                         #If polynomial kernel selected for svm classifier:
                                         conditionalPanel(condition ="input.CL_svm___p___kernel == 'polynomial'",
                                                          numericInput("CL_svm___p___degree",
                                                                       "Degree of polynomial",
                                                                       value = 3,
                                                                       min = 2,
                                                                       max  = 10)),
                                         #If radial or polynomial kernel selected for svm classifier:
                                         conditionalPanel(condition = "input.CL_svm___p___kernel == 'radial'|input.CL_svm___p___kernel == 'polynomial'",
                                                          numericInput("CL_svm___p___coef0",
                                                                       "Coef0",
                                                                       0)),
                                         #If radial or polynomial kernel selected for svm classifier:
                                         conditionalPanel(condition = "input.CL_svm___p___kernel == 'radial'|input.CL_svm___p___kernel == 'polynomial'|input.CL_svm___p___kernel == 'sigmoid'",
                                                          numericInput("CL_svm___p___gamma",
                                                                       "Gamma",
                                                                       NULL)) # Elisa default
                                         )
                        )
                    ),
                    #Elisa - should I include return_decision_values for CL's?
                    # Yes with default of it being checked

                    #Fourth decoding tab: feature processors
                    tabPanel(
                      title = "Feature Preprocessors",
                      width = NULL,
                      solidHeader = TRUE, status = "primary",
                      #Select feature processors (multi select)
                      uiOutput("FP_type"),
                      #Select additional parameters if user chooses fp_select_k_features
                      conditionalPanel(condition = "'fp_select_k_features' %in% input.FP_type",
                                       uiOutput("FP_skf___p___num_sites_to_use"),
                                       uiOutput("FP_skf___p___num_sites_to_exclude"))
                      ),

                    #Fifth decoding tab: result metrics
                    tabPanel(
                       title = "Result Metrics",
                       width = NULL,
                       solidHeader = TRUE, status = "primary",
                       #Select result metrics type (multi select)
                       uiOutput("RM_type"),
                       #Select additional parameters if user chooses rm_main_results
                       conditionalPanel(condition = "'rm_main_results' %in% input.RM_type",
                                        htmlOutput("RM_mr___np___include_norm_rank_results_text"),
                                        uiOutput("RM_mr___p___include_norm_rank_results")),
                       #Select additional parameters if user chooses RM_cm___np___text
                       conditionalPanel(condition = "'rm_confusion_matrix' %in% input.RM_type",
                                        htmlOutput("RM_cm___np___text"),
                                        uiOutput("RM_cm___p___save_TCD_results"),
                                        uiOutput("RM_cm___p___create_decision_vals_confusion_matrix"))

                      ),

                    #Sixth decoding tab: cross validator
                    tabPanel(
                      title = "Cross Validator",
                      width = NULL,
                      solidHeader = TRUE, status = "primary",
                      #Select cross validator parameters
                      checkboxInput("CV___p___run_TCD", "Test only at training times?",FALSE),
                      uiOutput("CV___p___num_resample_runs"),
                      uiOutput("CV___p___num_parallel_cores"),
                      #Include parallel outfile parameter if conditions met (as defined by online reference)
                      conditionalPanel(condition = "input.CV___p___num_parallel_cores >= 1 | !input.CV___p___num_parallel_cores",
                                       uiOutput("CV___p___parallel_outfile"))
                    ),

                    #Seventh decoding tab: runing analysis and generating scripts
                    tabPanel(
                      title = "Run Analysis",
                      width = NULL,
                      fluidRow(
                        column(
                          width = 4,
                          box(title = "Create a new script",
                              width = NULL,
                              status = "danger",
                              solidHeader = TRUE,
                              #Select script type
                              radioButtons("DC_script_mode", "File type for generated script", c("R", "R Markdown", "Matlab"), selected = "R"),
                              #Option to include comments
                              checkboxInput("include_comments", "Add code comments"),
                              #Running
                              uiOutput("DC_offer_scriptize"),
                              uiOutput("DC_offer_run_decoding"))  # Get strange errors if I try to add more UI elements :(
                        ),
                        column(width = 8, box(width = NULL, uiOutput("DC_ace")))
                      )
                    ),

                    #Eighth decoding tab: viewing the pdf output of running the script
                    tabPanel(
                      title = "View Results",
                      width = NULL,
                      solidHeader = TRUE, status = "primary",
                      uiOutput("DC_plot_pdf")
                    ),

             )
      )
    )
  )
)

### Plot decoding results ----
plot_decoding <- tabPanel(
  title = "Plot decoding results",
  column(width = 12,
         box(
           title = "Choose the result to plot",
           width = NULL,
           status = "danger",
           solidHeader = TRUE,
           shinyFiles::shinyFilesButton("Plot_chosen_result",
                                        "Browse", "", multiple = FALSE),
           helpText("Loaded result: "),
           textOutput("Plot_show_chosen_result")
           ),
         tabBox(width = NULL,
                tabPanel("Timeseries",
                         selectInput("Plot_timeseries_result_type",
                                     "Type of result to plot",
                                     all_result_type),
                         plotOutput("Plot_timeseries")
                         ),
                tabPanel("TCT heatmap",
                         selectInput("Plot_tct_result_type",
                                     NULL,
                                     all_result_type),
                         plotOutput("Plot_tct")
                ),
                tabPanel("PDF of script and result",
                         actionButton("Plot_create_pdf", "Create"),
                         helpText(""),
                         uiOutput("Plot_pdf")
                         )
                )
         )
  )


### Uploading new binned data ----
upload_binned <-  tabPanel(
  title = "Upload new binned data",
  width = NULL,
  box(title = NULL,
    width = NULL,
    uiOutput("DS_offer_upload_bin")
    )
  )

# Combining sidebar bodies ----
body <- dashboardBody(tabItems(tabItem(tabName = "bin", navbarPage(title = "",
                                                                   binning_params,
                                                                   plot_raster,
                                                                   upload_raster)),
                               tabItem(tabName = "decode", navbarPage(title = "",
                                                                      decoding_analysis,
                                                                      plot_decoding,
                                                                      upload_binned))
                               )
                      )



# Dashboard page output -----
dashboardPage(skin = "green",
              dashboardHeader(title = "NeuroShiny"),
              sidebar,
              body)

