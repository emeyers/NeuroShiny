
# Tab for data upload and data source parameters
data_source_tab <-
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
  # Select DS
  selectInput("DS_type", "Type of data source", c("ds_basic", "ds_generalization")),
  # DS Basic options
  conditionalPanel(condition = "input.DS_type == 'ds_basic'",
                   uiOutput("DS_basic___p___list_of_labels"),
                   checkboxInput("DS_basic___np___select_levels",
                                 "Select your specific levels", FALSE),
                   checkboxInput("DS_basic___np___advanced",
                                 "Select advanced parameters", FALSE)),
  # Option to specify levels for DS Basic
  conditionalPanel(condition = "input.DS_basic___np___select_levels && input.DS_type == 'ds_basic'",
                   uiOutput("DS_basic___np___list_of_levels_to_use")),
  # Optional advanced parameters for DS Basic
  conditionalPanel(condition = "input.DS_basic___np___advanced && input.DS_type == 'ds_basic'",
                   uiOutput("DS_basic___p___use_count_data"),
                   uiOutput("DS_basic___p___site_IDs_to_use"),
                   uiOutput("DS_basic___p___site_IDs_to_exclude"),
                   uiOutput("DS_basic___p___randomly_shuffled_labels"),
                   uiOutput("DS_basic___p___create_simultaneous_populations")),
  #DS Generalization options
  conditionalPanel(condition = "input.DS_type == 'ds_generalization'",
                   uiOutput("DS_gen___np___list_of_labels"),
                   #uiOutput("DS_gen___np___select_num_of_groups"), elisa
                   numericInput("DS_gen___np___class_number",
                                "How many classes?", 2, min = 2),
                   uiOutput("DS_gen___p___label_levels"),
                   checkboxInput("DS_gen___np___advanced",
                                 "Select advanced parameters", FALSE)),
  # Optional advanced parameters for DS Generalization
  conditionalPanel(condition = "input.DS_gen___np___advanced && input.DS_type == 'ds_generalization'",
                   uiOutput("DS_gen___p___use_count_data"),
                   uiOutput("DS_gen___p___site_IDs_to_use"),
                   uiOutput("DS_gen___p___site_IDs_to_exclude"),
                   uiOutput("DS_gen___p___randomly_shuffled_labels"),
                   uiOutput("DS_gen___p___create_simultaneous_populations"))
)
