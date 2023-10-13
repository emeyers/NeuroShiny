
################################################################################
######################## Libraries and removed functions #######################
################################################################################

library('shinydashboard')
library('NeuroDecodeR')
library('gh')
library('ggplot2')
library('dplyr')
library('shinyAce')
library('plotly')
library('shinycssloaders')
library('shinyFiles')
library('DT')
library('pdftools')
library('tools')
rm(list=ls())

################################################################################
################################### App setup ##################################
################################################################################

# https://groups.google.com/forum/#!msg/shiny-discuss/rU3vwGMZexQ/zeKhiYXrtEQJ
# Setting the uploaded file size limit to 1 GB
options(shiny.maxRequestSize=1000*1024^2)

################################################################################
############################### Hard-coded lists ###############################
################################################################################

# Prefixes of data to check valid types
multi_result_prefix <- list("analysis_ID", "result_name", "ds_", "cv_", "cl_", "fp_", "rm_")
single_result_prefix <- list("rm_main_results", "rm_confusion_matrix", "cross_validation_paramaters")

# List of inputs for decoding results
all_result_type <- c("zero_one_loss", "normalized_rank", "decision_vals", "all")
cm_result_type <- c("zero_one_loss", "mutual_information", "decision_vals")

# List of classifiers, feature processors, and result metrics
all_cl <- c("cl_max_correlation", "cl_svm", "cl_poisson_naive_bayes")
all_fp <- c("fp_zscore", "fp_select_k_features")
all_rm <- c("rm_main_results", "rm_confusion_matrix")

# Makes sure that fp_zscore is not used with the cl_poisson_naive_bayes
# Used in reactive_all_fp_avail
cl_fp <- data.frame(c(1, 1), c(1, 1), c(1, 0))
colnames(cl_fp) <- all_cl
rownames(cl_fp) <- all_fp

################################################################################
########################### Additional functions ###############################
################################################################################

# Function used to save generated script
move_file <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive = TRUE)
  file.rename(from = from, to = to)
}

# A cheat function for simple HTML rendering
html2Text <- function(string) {
  formatStr <- paste0("<br><font size='+1' color='black'><strong>",
                      string, "</strong></font>")
  return(helpText(HTML(formatStr)))
}

`%!in%` = Negate(`%in%`)

source("generate_scripts_from_shiny_params.R")
source("create_analysis_project.R")


################################################################################
####################### Other global variables #################################
################################################################################

app_base_dir <- getwd()  # "."  # "~"


