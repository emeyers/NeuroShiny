# I am definitely using these
library('shinydashboard')
library('NeuroDecodeR')

# I am using these but might abandon these
library('plotrix')
library('fields')

# I should use these
library('ggplot2')
library('dplyr')
library('plotly')

rm(list=ls())

# https://groups.google.com/forum/#!msg/shiny-discuss/rU3vwGMZexQ/zeKhiYXrtEQJ
# setting the uploaded file size limit to 1 GB
options(shiny.maxRequestSize=1000*1024^2)


projects_folder_path <- file.path("..", "decoding_analysis_projects")
project_name <- "Katsuki_Constantinidis_popout_R_analysis"


#project_basedir <- trimws(file.path(here::here(), projects_folder_path, project_name, " "))
project_basedir <- trimws(file.path(projects_folder_path, project_name, " "))

raster_base_dir <- paste0(project_basedir, trimws(file.path('data', ' ')))
binned_base_dir <- paste0(project_basedir, trimws(file.path('data','binned_data', ' ')))
result_base_dir <- paste0(project_basedir, trimws(file.path('results', 'decoding_results', 'decoding_result_files', ' ')))
www_base_dir <- trimws(file.path('www', ' '))


script_base_dir <- "scripts"  # is this needed?



req_dc_para <- c("DS_binned_data", "DS_type")

#List of classifiers, feature processors, and result metrics
all_cl <- c("cl_max_correlation", "cl_svm", "cl_poisson_naive_bayes")
all_fp <- c("fp_zscore", "fp_select_k_features")
all_rm <- c("rm_main_results", "rm_confusion_matrix")
all_result_type <- c("zero_one_loss", "normalized_rank", "decision_values", "all")


#Makes sure that fp_zscore is not used with the cl_poisson_naive_bayes
#Used in reactive_all_fp_avail
df_cl_fp <- data.frame(c(1, 1), c(1, 1), c(1, 0))
colnames(df_cl_fp) <- all_cl
rownames(df_cl_fp) <- all_fp

#Function used to save generated script
move_file <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}

preprocess_paras <- function(decoding_params){
  decoding_params$DC_to_be_saved_result_name <- paste0(result_base_dir, trimws(file.path(decoding_params$DC_to_be_saved_result_name)))
  #decoding_params$DC_to_be_saved_result_name <- here::here(result_base_dir,decoding_params$DC_to_be_saved_result_name)
  return(decoding_params)
}


source("generate_scripts_from_shiny_params.R")

