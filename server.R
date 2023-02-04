
myserver <- function(input,output,session){
################################################################################
############################### Dependent tabs #################################
################################################################################

  # Menu for Binning Data
  output$menuBin <- renderMenu({
    # Open when project is selected
    if (is.list(input$project_folder)){
      menuItem("Binning the raster data", tabName = "bin")
    }
  })

  # Menu for
  output$menuDecode <- renderMenu({
    if (is.list(input$project_folder)){
      menuItem("Population decoding", tabName = "decode")
    }
  })

################################################################################
############################## Reactive values #################################
################################################################################

  rv <- reactiveValues()

  # Raster reactive values
  rv$raster_base_dir <- NULL
  rv$raster_cur_dir_name <- NULL
  rv$raster_bMat <-FALSE
  rv$raster_num_neuron <- NA
  rv$raster_bRda <- FALSE
  rv$raster_cur_neuron <- 1
  rv$raster_cur_file_name <- NULL

  # Binned data reactive values
  rv$create_bin_function_run <- ""
  rv$mRaster_cur_data <- NULL
  rv$binned_base_dir <- NULL
  rv$binned_file_name <- NA
  rv$binned_labels <- NULL
  rv$displayed_script <- ""

  # Decoding results reactive values
  rv$result_base_dir <- NULL
  rv$result_chosen <- NA
  rv$result_data <- NULL
  rv$prev_bins <- NULL
  rv$manifest_chosen <- NULL
  rv$manifest_data <- NULL
  rv$manifest_legend_names <- NULL
  rv$save_script_name <- "scripts"
  # Find which ids to signal eventReactive to check if they are in position
  rv$decoding_para_id_computed <- 1

################################################################################
############################ Source server files ###############################
################################################################################

  # Server files for "Binning the Raster Data" tab
  source("servers/server_select_project.R", local = TRUE)
  source("servers/server_binning_params.R", local = TRUE)
  source("servers/server_plot_raster.R", local = TRUE)
  source("servers/server_upload_new_raster.R", local = TRUE)

  # Server files for "Population Decoding"
  source("servers/server_data_source.R", local = TRUE)
  source("servers/server_split_parameters.R", local = TRUE)
  source("servers/server_fp.R", local = TRUE)
  source("servers/server_rm.R", local = TRUE)
  source("servers/server_cv.R", local = TRUE)
  source("servers/server_run_analysis.R", local = TRUE)
  source("servers/server_current_results.R", local = TRUE)
  source("servers/server_plot_single_result.R", local = TRUE)
  source("servers/server_plot_multi_result.R", local = TRUE)

}
