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
# setting the uploaded file size limit to 1 GB
options(shiny.maxRequestSize=1000*1024^2)


#state_base_dir <- here::here()
#raster_base_dir <- here::here('data','raster')
#binned_base_dir <- here::here('data','binned')
#result_base_dir <- here::here('results')
#www_base_dir <- here::here('www')
#script_base_dir <- "scripts"


# going to use relative paths rather than the here() package
state_base_dir <- trimws(file.path('.', ' '))
raster_base_dir <- paste0(state_base_dir, trimws(file.path('data','raster', ' ')))
binned_base_dir <- paste0(state_base_dir, trimws(file.path('data','binned', ' ')))
result_base_dir <- paste0(state_base_dir, trimws(file.path('results', ' ')))
www_base_dir <- paste0(state_base_dir, trimws(file.path('www', ' ')))
script_base_dir <- "scripts"



req_dc_para <- c("DS_binned_data", "DS_type")


all_cl <- c("cl_max_correlation", "cl_svm", "cl_poisson_naive_bayes")
all_fp <- c("fp_zscore", "fp_select_k_features")
all_rm <- c("rm_main_results", "rm_confusion_matrix")

# makes sure that fp_zscore is not used with the cl_poisson_naive_bayes
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


source("generate_r_script_from_shiny_decoding_params.R")




