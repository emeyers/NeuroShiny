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
  decoding_params = decoding_params[!is.na(decoding_params)]
  decoding_params = preprocess_paras(decoding_params)

  my_text = "\n"
  my_text = paste0(my_text, "binned_data <- '",decoding_params$DS_binned_data$files$`0`[[2]],"' \n\n\n")

  my_text = paste0(my_text, "ds <- NeuroDecodeR::", decoding_params$DS_type,"(",
                   "binned_data = binned_data,\n")

  for(element in names(decoding_params)){
    if(startsWith(element, "DS_basic_")){
      val <- eval(str2lang(paste0("decoding_params$",element)))
      my_text = paste0(my_text, gsub("DS_basic_", "", element)," = ", val, ",\n")
    }
    if(startsWith(element, "DS_gen_")){
      val <- eval(str2lang(paste0("decoding_params$",element)))
      my_text = paste0(my_text, gsub("DS_gen_", "", element)," = ", val, ",\n")
    }
  }
  my_text = paste0(substring(my_text,1, nchar(my_text)-2), ") \n\n")

  #Classifier
  my_text = paste0(my_text, "cl <- NeuroDecodeR::", decoding_params$CL_type,"(")
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



  #Cross Validator
  my_text = paste0(my_text, "cv <- NeuroDecodeR::cv_standard(")
  for(element in names(decoding_params)){
    if(startsWith(element, "CV_")){
      val <- eval(str2lang(paste0("decoding_params$",element)))
      my_text = paste0(my_text, gsub("CV_", "", element)," = ", val, ",\n")
    }
  }

  my_text = paste0(substring(my_text,1, nchar(my_text)-2), ") \n\n")

  return(my_text)


}


