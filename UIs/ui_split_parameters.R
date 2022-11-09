

split_parameters_tab <-
  tabPanel(
  title = "DS Split Parameters",
  width = NULL,
  solidHeader = TRUE, status = "primary",
  fluidRow(
    column(
      width = 10,
      #Output the max rep (text output for backend calculation)
      uiOutput("DS___np___max_repetition_avail_with_any_site"),
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
                       #uiOutput("DS_show_chosen_repetition_info"), #TODO makes everything crash
                       uiOutput("DS_gen___p___num_resample_sites")
      ),
      #plot output for number of sites against number of repeated conditions
      withSpinner(plotlyOutput("DS_show_level_repetition_info"),
                  color = "#79c9da")
    )
  )
)
