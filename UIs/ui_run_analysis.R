
# Run, save, show analysis after selecting values
run_analysis_tab <-
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
            # Select script type
            actionButton("restore_last_script", "Restore previous script?"),
            helpText(" "),
            radioButtons("DC_script_mode",
                         "File type for generated script",
                         c("R", "R Markdown", "Matlab"),
                         selected = "R"),
            # Option to include comments
            checkboxInput("include_comments", "Add code comments"),
            # Option to change result name
            textInput("result_name", "Optional: add result name", ""),
            #Running
            actionButton("DC_run_script", "Run and save the script"),
            uiOutput("DC_scriptize_error"),
            helpText(" "),
            actionButton("DC_save_decoding", "Save the script only"))),
      column(width = 8,
             box(width = NULL,
                 uiOutput("DC_ace")))
    )
  )
