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


state_base_dir <- here::here()
raster_base_dir <- here::here('data','raster')
binned_base_dir <- here::here('data','binned')
result_base_dir <- here::here('results')
www_base_dir <- here::here('www')
script_base_dir <- "scripts"


req_dc_para <- c("DS_binned_data", "DS_type")


all_cl <- c("cl_max_correlation", "cl_svm", "cl_poisson_naive_bayes")
all_fp <- c("fp_select_k_features", "fp_zscore")
#all_result_type <- c("zero_one_loss", "normalized_rank", "decision_values", "all")
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







# convert the selected parameters to an R script
create_script_in_r <- function(decoding_params) {
  decoding_params = decoding_params[!is.na(decoding_params)]
  decoding_params = preprocess_paras(decoding_params)

  my_text <- "library(NeuroDecodeR)\n\n"
  my_text = paste0(my_text, "binned_data <- '",decoding_params$DS_binned_data$files$`0`[[2]],"' \n\n")

  my_text = paste0(my_text, "ds <- ", decoding_params$DS_type,"(\n",
                   "\tbinned_data = binned_data,\n")

  for(element in names(decoding_params)){
    if(startsWith(element, "DS_basic_")){
      val <- eval(str2lang(paste0("decoding_params$",element)))
      my_text = paste0(my_text, "\t", gsub("DS_basic_", "", element)," = ", val, ",\n")
    }
    if(startsWith(element, "DS_gen_")){
      val <- eval(str2lang(paste0("decoding_params$",element)))
      my_text = paste0(my_text, "\t", gsub("DS_gen_", "", element)," = ", val, ",\n")
    }
  }
  my_text = paste0(substring(my_text, 1, nchar(my_text)-2), ") \n\n")

  #Classifier
  my_text = paste0(my_text, "cl <- ", decoding_params$CL_type,"(")
  for(element in names(decoding_params)){
    if(startsWith(element, "CL_svm")){
      val <- eval(str2lang(paste0("decoding_params$",element)))
      my_text = paste0(my_text, gsub("CL_svm_", "", element)," = ", val, ",\n")
    }
  }

  if(decoding_params$CL_type == 'cl_svm'){
    my_text = paste0(substring(my_text,1, nchar(my_text)-2), ") \n\n")
  }else{
    my_text = paste0(my_text, ") \n\n")
  }



  #Feature Preprocessors
  fp_zs <- NULL
  fp_skf <- NULL
  fp_list <- NULL


  for(element in names(decoding_params)){
    if(startsWith(element, "FP_type")){
      if("fp_zscore" %in% decoding_params$FP_type){
        fp_zs <- "fp_zs <- fp_zscore()\n\n"
        fp_list = paste0(fp_list,"fp_zs,")
      }
      if("fp_select_k_features" %in% decoding_params$FP_type){
        fp_skf <- "fp_skf <- fp_select_k_features("
      }
    }
    if(startsWith(element, "FP_skf_")){
      val <- eval(str2lang(paste0("decoding_params$",element)))
      fp_skf = paste0(fp_skf, gsub("FP_skf_", "", element)," = ", val, ",\n")
    }
  }


  if(!is.null(fp_skf)){
    fp_skf = paste0(substring(fp_skf,1, nchar(fp_skf)-2), ") \n\n")
    fp_list = paste0(fp_list,"fp_skf,")
  }

  my_text = paste0(my_text,fp_skf,fp_zs)
  fp_list = paste0(substring(fp_list,1, nchar(fp_list)-1), ") \n\n")
  my_text = paste0(my_text, "fps <- list(",fp_list)




  # Result metrics

  rm_main_text <- NULL
  rm_cm_text <- NULL


  if("rm_main_results" %in% decoding_params$RM_type){
    rm_main_text <- paste0("rm_main <- rm_main_results(\n\t include_norm_rank_results = ",
                           as.character(decoding_params$RM_mr_include_norm_rank_results), ")\n")
  }

  if("rm_confusion_matrix" %in% decoding_params$RM_type){
    rm_cm_text <- paste0("rm_cm <- rm_confusion_matrix(\n\t save_only_same_train_test_time = ",
                         as.character(decoding_params$RM_cm_save_only_same_train_test_time), ",\n\t ",
                         "create_decision_vals_confusion_matrix = ",
                         as.character(decoding_params$RM_cm_create_decision_vals_confusion_matrix), ")\n")
  }


  # create text with a list that has the results metrics in them
  rm_list_text <- "rms <- list("
  if(!is.null(rm_main_text)){
    rm_list_text <- paste0(rm_list_text, "rm_main")
  }
  if(!is.null(rm_cm_text)){

    if(!is.null(rm_main_text)){
      rm_list_text <- paste0(rm_list_text, ", ")
    }

    rm_list_text <- paste0(rm_list_text, "rm_cm")
  }
  rm_list_text <- paste0(rm_list_text, ")\n\n")


  my_text <-  paste0(my_text, rm_main_text, rm_cm_text,  rm_list_text)



  #Cross Validator
  my_text = paste0(my_text, "cv <- cv_standard(\n\t datasource = ds, \n")
  my_text = paste0(my_text,"\t classifier = cl, \n\t feature_preprocessors = fps, \n")
  for(element in names(decoding_params)){
    if(startsWith(element, "CV_")){
      val <- eval(str2lang(paste0("decoding_params$",element)))
      my_text = paste0(my_text, "\t ", gsub("CV_", "", element)," = ", val, ",\n")
    }
  }

  my_text = paste0(substring(my_text,1, nchar(my_text)-2), ") \n\n")

  return(my_text)


}


