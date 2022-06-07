# Sidebar tabs----
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs", menuItem("Population Decoding", tabName = "decode")
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
                    tabPanel(
                      title = "Data Source",
                      width = NULL,
                      solidHeader = TRUE,
                      status = "primary",
                      box(width = NULL,
                          title = "Choose a binned data file",
                          status = "danger",
                          solidHeader = TRUE,
                          shinyFiles::shinyFilesButton("DS_binned_data", "Browse", "", multiple = FALSE),
                          helpText("Loaded binned data: "),
                          textOutput("DS_show_chosen_bin")),
                      selectInput("DS_type", "Type of data source", c("ds_basic","ds_generalization")),
                      conditionalPanel(condition = "input.DS_type == 'ds_basic'",
                                       uiOutput("DS_basic_list_of_var_to_decode"),
                                       checkboxInput("DS_basic_use_all_levels", "Select your specific levels", FALSE),
                                       checkboxInput("DS_basic_advanced", "Select advanced parameters?", FALSE)
                      ),
                      conditionalPanel(condition = "input.DS_basic_use_all_levels && input.DS_type == 'ds_basic'",
                                       uiOutput("DS_basic_list_of_levels_to_use")
                      ),
                      conditionalPanel(condition = "input.DS_basic_advanced && input.DS_type == 'ds_basic'",
                                       uiOutput("DS_basic_use_count_data"),
                                       uiOutput("DS_basic_site_IDs_to_use"),
                                       uiOutput("DS_basic_site_IDs_to_exclude"),
                                       uiOutput("DS_basic_randomly_shuffled_labels"),
                                       uiOutput("DS_basic_create_simultaneous_populations")
                      ),
                      conditionalPanel(condition = "input.DS_type == 'ds_generalization'",
                                       uiOutput("DS_gen_list_of_var_to_decode"),
                                       uiOutput("DS_gen_select_num_of_groups"),
                                       numericInput("DS_gen_class_number","How many classes?", 2, min = 2),
                                       #splitLayout(
                                       uiOutput("DS_gen_label_levels"),
                                         #uiOutput("DS_gen_test_label_levels"),
                                       checkboxInput("DS_gen_advanced", "Select advanced parameters?", FALSE)
                      ),
                      conditionalPanel(condition = "input.DS_gen_advanced && input.DS_type == 'ds_generalization'",
                                       uiOutput("DS_gen_use_count_data"),
                                       uiOutput("DS_gen_site_IDs_to_use"),
                                       uiOutput("DS_gen_site_IDs_to_exclude"),
                                       uiOutput("DS_gen_randomly_shuffled_labels"),
                                       uiOutput("DS_gen_create_simultaneous_populations")
                      )
                    ),

                    tabPanel(
                      title = "DS Split Parameters",
                      width = NULL,
                      solidHeader = TRUE, status = "primary",
                      fluidRow(
                        column(
                          width = 10,
                          uiOutput("DS_max_repetition_avail_with_any_site"),
                          conditionalPanel(condition = "input.DS_type == 'ds_basic'",
                                           uiOutput("DS_basic_num_cv_splits"),
                                           uiOutput("DS_basic_num_label_repeats_per_cv_split"),
                                           uiOutput("DS_show_chosen_repetition_info"),
                                           uiOutput("DS_basic_num_resample_sites")
                          ),
                          conditionalPanel(condition = "input.DS_type == 'ds_generalization'",
                                           uiOutput("DS_gen_num_cv_splits"),
                                           uiOutput("DS_gen_num_label_repeats_per_cv_split"),
                                           #uiOutput("DS_show_chosen_repetition_info"),
                                           uiOutput("DS_gen_num_resample_sites")
                          ),
                          plotlyOutput("DS_show_level_repetition_info")
                        )
                      )
                    ),

                    tabPanel(
                      title = "Classifier",
                      width = NULL,
                      solidHeader = TRUE, status = "primary",
                      selectInput("CL_type", "Classifier", all_cl),
                      box(
                        width = NULL,
                        title = "Additional parameters (if applicable)",
                        conditionalPanel(condition  = "input.CL_type == 'cl_svm'",
                                         selectInput("CL_svm_kernel",
                                                     "Kernel",
                                                     c("linear", "polynomial", "radial", "sigmoid"),
                                                     selected = "linear"),
                                         numericInput("CL_svm_cost",
                                                      "Cost",
                                                      value = 1, min = 0),
                                         conditionalPanel(condition ="input.CL_svm_kernel == 'polynomial'",
                                                          numericInput("CL_svm_degree",
                                                                       "Degree of polynomial",
                                                                       value = 3,
                                                                       min = 2,
                                                                       max  = 10)),
                                         conditionalPanel(condition = "input.CL_svm_kernel == 'radial'|input.CL_svm_kernel == 'polynomial'",
                                                          numericInput("CL_svm_coef0",
                                                                       "Coef0",
                                                                       0)),
                                         conditionalPanel(condition = "input.CL_svm_kernel == 'radial'|input.CL_svm_kernel == 'polynomial'|input.CL_svm_kernel == 'sigmoid'",
                                                          numericInput("CL_svm_gamma",
                                                                       "Gamma",
                                                                       NULL))
                                         )
                        )
                    ),


                    tabPanel(
                      title = "Feature Preprocessors",
                      width = NULL,
                      solidHeader = TRUE, status = "primary",
                      uiOutput("FP_type"),
                      conditionalPanel(condition = "'fp_select_k_features' %in% input.FP_type",
                                       uiOutput("FP_skf_num_site_to_use"),
                                       uiOutput("FP_skf_num_sites_to_exclude"))
                      ),


                    tabPanel(
                       title = "Result Metrics",
                       width = NULL,
                       solidHeader = TRUE, status = "primary",
                       uiOutput("RM_type"),
                       conditionalPanel(condition = "'rm_main_results' %in% input.RM_type",
                                        htmlOutput("RM_mr_include_norm_rank_results_text"),
                                        uiOutput("RM_mr_include_norm_rank_results")),
                       conditionalPanel(condition = "'rm_confusion_matrix' %in% input.RM_type",
                                        htmlOutput("RM_confusion_matrix_text"),
                                        uiOutput("RM_cm_save_only_same_train_test_time"),
                                        uiOutput("RM_cm_create_decision_vals_confusion_matrix"))

                      ),



                    tabPanel(
                      title = "Cross Validator",
                      width = NULL,
                      solidHeader = TRUE, status = "primary",
                      checkboxInput("CV_test_only_at_training_time", "Test only at training times?",FALSE),
                      uiOutput("CV_num_resample_runs"),
                      uiOutput("CV_num_parallel_cores"),
                      conditionalPanel(condition = "input.CV_num_parallel_cores >= 1 | !input.CV_num_parallel_cores",
                                       uiOutput("CV_parallel_outfile"))
                    ),

                    tabPanel(
                      title = "Run Analysis",
                      width = NULL,
                      fluidRow(
                        column(
                          width = 6,
                          box(title = "Create a new script",
                              width = NULL,
                              status = "danger",
                              solidHeader = TRUE,
                              radioButtons("DC_script_mode", "File type for generated script", c("R", "R Markdown", "Matlab"), selected = "R"),
                              uiOutput("DC_offer_scriptize"),
                              uiOutput("DC_offer_run_decoding"))
                        ),
                        column(width = 6, box(width = NULL, uiOutput("DC_ace")))
                      )
                    )
             )
      )
    )
  )
)

## Combining sidebar bodies ----
body <- dashboardBody(tabItems(tabItem(tabName = "decode",
                                       navbarPage(title = "",
                                                  decoding_analysis))
)
)



# Dashboard page output -----
dashboardPage(skin = "green",
              dashboardHeader(title = "NeuroShiny"),
              sidebar,
              body)
