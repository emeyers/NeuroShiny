
# Upload new raster data under binning raster tab
upload_new_raster <- tabPanel(
  title = "Upload new raster data",
  fluidRow(box(width = NULL,
               fileInput("bin_uploaded_raster",
                         "Upload a zipped file raster data",
                         multiple = TRUE,
                         accept = c(".zip")),
               uiOutput("bin_uploaded_raster_location"),
               actionButton("bin_save_raster_to_disk", "Save to disk"),
               helpText(""),
               uiOutput("upload_error"))
  )
)

