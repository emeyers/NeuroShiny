
myServer <- function(input,output,session){

################################################################################
############################### Side menu tabs #################################
################################################################################

  # Side menu for binning data
  output$menu_bin <- renderMenu({
    # Open when project is selected
    if (!is.null(rv$working_dir)){
      menuItem("Binning the raster data", tabName = "bin")
    }
  })

  # Side menu for decoding
  output$menu_decode <- renderMenu({
    if (!is.null(rv$working_dir)){
      menuItem("Population decoding", tabName = "decode")
    }
  })

################################################################################
############################## Reactive values #################################
################################################################################

  rv <- reactiveValues()

  rv$base_dir <- NULL
  rv$projects_available <- list.dirs(file.path(app_base_dir, "projects"),
                                     full.names = FALSE, recursive = FALSE)

  # Raster reactive values
  rv$raster_base_dir <- NULL
  rv$selected_rasters <- NULL
  rv$raster_num_neuron <- 0
  rv$raster_cur_neuron <- 1
  rv$raster_cur_file_name <- NULL

  # Binned data reactive values
  rv$folders_to_bin <- NULL
  rv$create_bin_function_run <- ""
  rv$cur_raster_matrix <- NULL
  rv$binned_base_dir <- NULL
  rv$binned_file_name <- NULL
  rv$binned_labels <- NULL
  rv$displayed_script <- ""
  rv$bin_full_raster_path <- NULL
  rv$raster_list <- NULL
  rv$selected_rasters <- NULL

  # Decoding results reactive values
  rv$result_base_dir <- NULL
  rv$decoding_results_base_dir <- NULL
  rv$binned_data_to_decode <- NULL
  rv$result_chosen <- NULL
  rv$result_data <- NULL
  rv$prev_bins <- NULL
  rv$script_error_message <- NULL
  rv$valid_result_list <- c()
  rv$valid_manifest_list <- c()
  rv$manifest_chosen <- NULL
  rv$manifest_data <- NULL
  rv$manifest_legend_names <- NULL
  rv$save_script_name <- "scripts"
  # Find which ids to signal eventReactive to check if they are in position
  rv$decoding_para_id_computed <- 1

################################################################################
############################ Server source files ###############################
################################################################################

  # Server files for project changes
  source("servers/server_select_project.R", local = TRUE)
  source("servers/server_github.R", local = TRUE)

  # Server files for "Binning the Raster Data" tab
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
