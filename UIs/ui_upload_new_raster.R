
### Upload new raster data ----
upload_raster <- tabPanel(
  title = "Upload new raster data",
  fluidRow(box(width = NULL,
               helpText("We only accept .mat and .Rda format"),
               uiOutput("bin_offer_upload_raster"),
               uiOutput("bin_offer_create_raster"),
               uiOutput("bin_evil_raster"),
               textOutput("bin_show_create_raster_function_run")
  )
  )
)



