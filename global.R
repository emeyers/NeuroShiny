# I am definitely using these
library('shinydashboard')
library("NeuroDecodeR")
library('here')

# I am using these but might abandon these
library("plotrix")
library('fields')

# I should use these
library('ggplot2')
library('dplyr')
library('plotly')

rm(list=ls())

# https://groups.google.com/forum/#!msg/shiny-discuss/rU3vwGMZexQ/zeKhiYXrtEQJ
# settng the uploaded file size limit to 1 GB
options(shiny.maxRequestSize=1000*1024^2)


state_base_dir <- here::here()
raster_base_dir <- here::here('data','raster')
binned_base_dir <- here::here('data','binned')
result_base_dir <- here::here('results')
www_base_dir <- here::here('www')
script_base_dir <- "scripts"


req_dc_para <- c("DS_binned_data", "DS_type")


all_cl <- c("cl_max_correlation", "cl_svm", "cl_poisson_naive_bayes")
all_fp <- c("fp_select_k_features", "fp_zscore")
all_result_type <- c("zero_one_loss", "normalized_rank", "decision_values", "all")

df_cl_fp <- data.frame(c(1, 1), c(1, 1), c(1, 0))

colnames(df_cl_fp) <- all_cl
rownames(df_cl_fp) <- all_fp



move_file <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}

preprocess_paras <- function(decoding_params){
  decoding_params$DC_to_be_saved_result_name <- here::here(result_base_dir,decoding_params$DC_to_be_saved_result_name)
  return(decoding_params)
}

create_script_in_r <- function(decoding_params) {

  print(decoding_params)
  #decoding_params = preprocess_paras(decoding_params)
  my_text = "\n"
  my_text = paste0(my_text, "binned_data <- '",decoding_params$DS_binned_data$files$`0`[[2]],"' \n\n\n")

  my_text = paste0(my_text, "ds <- NeuroDecodeR::", decoding_params$DS_type,"(",
                   "binned_data = binned_data,\n")

    if(decoding_params$DS_type == "ds_basic"){
      my_text = paste0(my_text, "var_to_decode = ", decoding_params$DS_basic_list_of_var_to_decode, ",\n")
      my_text = paste0(my_text, "num_label_repeats_per_cv_split = ", decoding_params$DS_basic_num_label_repeats_per_cv_split, ",\n")
      my_text = paste0(my_text,"num_cv_splits = ", decoding_params$DS_basic_num_cv_splits, ",\n")
      my_text = paste0(my_text, "num_resample_sites = ", decoding_params$DS_basic_num_resample_sites)

      if("DS_basic_use_all_levels" %in% colnames(decoding_params)){
        my_text = paste0(my_text, ",\n label_levels_to_use = ", decoding_params$DS_basic_list_of_levels_to_use)
      }

      if("DS_basic_use_count_data" %in% colnames(decoding_params)){
        my_text = paste0(my_text, ",\n use_count_data = ", decoding_params$DS_basic_use_count_data)
        my_text = paste0(my_text, ",\n site_IDs_to_use = ", decoding_params$DS_basic_site_IDs_to_use)
        my_text = paste0(my_text, ",\n site_IDs_to_exclude = ", decoding_params$DS_basic_site_IDs_to_exclude)
        my_text = paste0(my_text, ",\n randomly_shuffled_labels = ", decoding_params$DS_basic_randomly_shuffled_labels)
        my_text = paste0(my_text, ",\n create_simultaneous_populations = ", decoding_params$DS_basic_create_simultaneous_populations)
      }

      #ds_gen
    }else{
      my_text = paste0(my_text, "var_to_decode = ", decoding_params$DS_gen_list_of_var_to_decode, ",\n")
      #my_text = paste0(my_text, "temp = ", decoding_params$DS_gen_select_num_of_groups, ",\n")
      my_text = paste0(my_text, "train_label_levels = ", decoding_params$DS_gen_label_levels, ",\n")
      my_text = paste0(my_text, "num_label_repeats_per_cv_split = ", decoding_params$DS_gen_num_label_repeats_per_cv_split, ",\n")
      my_text = paste0(my_text, "num_cv_splits = ", decoding_params$DS_gen_num_cv_splits, ",\n")
      my_text = paste0(my_text, "num_resample_sites = ", decoding_params$DS_gen_num_resample_sites,)

      if("DS_gen_use_count_data" %in% colnames(decoding_params)){
        my_text = paste0(my_text, ",\n use_count_data = ", decoding_params$DS_gen_use_count_data)
        my_text = paste0(my_text, ",\n site_IDs_to_use = ", decoding_params$DS_gen_site_IDs_to_use)
        my_text = paste0(my_text, ",\n site_IDs_to_exclude = ", decoding_params$DS_gen_site_IDs_to_exclude)
        my_text = paste0(my_text, ",\n randomly_shuffled_labels = ", decoding_params$DS_gen_randomly_shuffled_labels)
        my_text = paste0(my_text, ",\n create_simultaneous_populations = ", decoding_params$DS_gen_create_simultaneous_populations)
      }

      my_text = paste0(my_text, ") \n\n")
    }

  my_text = paste0(my_text, "cl <- NeuroDecodeR::", decoding_params$CL_type,"(")
    #Classifier
    if(decoding_params$CL_type == 'cl_svm'){
      my_text = paste0(my_text, "kernel = ", decoding_params$CL_svm_kernel)
      my_text = paste0(my_text, ",\n cost = ", decoding_params$CL_svm_cost)
      if(decoding_params$CL_svm_kernel == 'polynomial'){
        my_text = paste0(my_text, ",\n degree = ", decoding_params$CL_svm_degree)
        my_text = paste0(my_text, ",\n coef0 = ", decoding_params$CL_svm_coef0)
        my_text = paste0(my_text, ",\n gamma = ", decoding_params$CL_svm_gamma)
      }
      if(decoding_params$CL_svm_kernel == 'radial'){
        my_text = paste0(my_text, ",\n coef0 = ", decoding_params$CL_svm_coef0)
        my_text = paste0(my_text, ",\n gamma = ", decoding_params$CL_svm_gamma)
      }
      if(decoding_params$CL_svm_kernel == 'sigmoid'){
        my_text = paste0(my_text, ",\n gamma = ", decoding_params$CL_svm_gamma)
      }
    }

    #Feature Preprocessors
    #rv_para$id <- c(rv_para$id, "FP_type")
    #if ('fp_select_k_features' %in% input$FP_type){
    #  rv_para$id <- c(rv_para$id, "FP_num_site_to_use", "FP_num_sites_to_exclude")
    #}

    #Cross Validator
    #rv_para$id <- c(rv_para$id, "CV_test_only_at_training_time",
    #                "CV_num_resample_runs", "CV_num_parallel_cores")
    #if(!is.null(input$CV_num_parallel_cores) && input$CV_num_parallel_cores >= 1){
    #  rv_para$id <- c(rv_para$id, "CV_parallel_outfile")
    #}


    return(my_text)


}


