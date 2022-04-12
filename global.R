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

preprocess_paras <- function(my_decoding_params){
  my_decoding_params$DC_to_be_saved_result_name <- here::here(result_base_dir,my_decoding_params$DC_to_be_saved_result_name)
  return(my_decoding_params)
}

create_script_in_r <- function(my_decoding_params) {
  my_decoding_params = preprocess_paras(my_decoding_params)
  my_text = "\n"
  my_text = paste0(my_text, "binned_data <- '",my_decoding_params$binned_data,"' \n\n\n")

  my_text = paste0(my_text, "ds <- NeuroDecodeR::", my_decoding_params$DS_type,"(",
                   "binned_data = binned_data,\n",
                   "var_to_decode = ", my_decoding_params$DS_var_to_decode, ",\n",
                   "num_cv_splits = ", my_decoding_params$DS_num_cv_splits, ",\n")

  if(my_decoding_params$DS_type == "ds_generalization"){
    my_text = paste0(my_text, "train_label_levels = ",my_decoding_params$DS_train_label_levels, ",\n",
                     "test_label_levels = ",my_decoding_params$DS_test_label_levels, ",\n")
  }

  my_text = paste0(my_text, "use_count_data = ",my_decoding_params$DS_use_count_data, ",\n",
                   "num_label_repeats_per_cv_split = ",my_decoding_params$DS_num_label_repeats_per_cv_split, ",\n" )

  #if(my_decoding_params$DS_type == "ds_basic"){
  #  my_decoding_params$DS_use_all_levels == FALSE
  #  if(my_decoding_params$DS_use_all_levels== TRUE){
  #    my_text = paste0(my_text, "label_levels_to_use = ", deparse(dput(my_decoding_params$label_levels_to_use)), ",\n")
  #  }else{
  #    my_text = paste0(my_text, "label_levels_to_use = NULL,\n")
  #  }
  #}
  print(my_decoding_params$DS_site_IDs_to_use)
  my_text = paste0(my_text, "num_resample_sites = ",my_decoding_params$DS_num_resample_sites, ",\n",
                   "site_IDs_to_use = ",my_decoding_params$DS_site_IDs_to_use, ",\n",
                   "site_IDs_to_exclude = ",my_decoding_params$DS_site_IDs_to_exclude, ",\n",
                   "randomly_shuffled_labels = ",my_decoding_params$DS_randomly_shuffled_labels, ",\n",
                   "create_simultaneous_populations = ",my_decoding_params$DS_create_simultaneous_populations, ")\n\n" )



  return(my_text)

}


