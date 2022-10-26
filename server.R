


myserver <- function(input,output,session){
  rv <- reactiveValues()

  # Raster reactive values
  rv$raster_base_dir <- raster_base_dir
  rv$raster_cur_dir_name <- NA
  rv$raster_bMat <-FALSE
  rv$raster_num_neuron <- NA
  rv$raster_bRda <- FALSE
  rv$raster_cur_neuron <- 1
  rv$raster_cur_file_name <- NULL

  # Binned data reactive values
  rv$create_bin_function_run <- ""
  rv$mRaster_cur_data <- NULL
  rv$binned_base_dir <- binned_base_dir
  rv$binned_file_name <- NA
  rv$binned_all_var <- NULL
  rv$displayed_script <- ""

  # Decoding results reactive variables
  rv$result_base_dir <- result_base_dir
  rv$result_chosen <- NA
  rv$result_data <- NULL
  rv$save_script_name <- NULL


  # Decoding rvs
  # decoding_para_id changes. This is used by observerEvent who figures out
  # the ids to signal eventReactive to check if they are in position
  rv$decoding_para_id_computed <- 1

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
  source("servers/server_plot_decoding.R", local = TRUE)

}
