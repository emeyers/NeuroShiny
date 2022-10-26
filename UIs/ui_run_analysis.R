

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
            #Select script type
            radioButtons("DC_script_mode",
                         "File type for generated script",
                         c("R", "R Markdown", "Matlab"),
                         selected = "R"),
            #Option to include comments
            checkboxInput("include_comments", "Add code comments"),
            #Running
            uiOutput("DC_offer_scriptize"),
            uiOutput("DC_offer_save_decoding"), # Get strange errors if I try to add more UI elements :(
            uiOutput("DC_plot_pdf"))
      ),
      column(width = 8,
             box(width = NULL,
                 uiOutput("DC_ace")
                 )
             )
    )
  )
