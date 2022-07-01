

# convert the selected parameters to an R script
generate_r_script_from_shiny_decoding_params <- function(decoding_params) {


  # remove any fields that are null
  decoding_params = decoding_params[!is.na(decoding_params)]


  include_comments <- decoding_params$include_comments



  ### Load the NDR and list the binned file name ------

  my_text <- "library(NeuroDecodeR)\n\n"

  if (include_comments) {
    my_text <- paste0(my_text, "\n# binned file name\n")
  }

  my_text <- paste0(my_text,
                    "binned_data <- '", decoding_params$binned_dir_name,
                    decoding_params$DS_binned_data$files$`0`[[2]], "' \n\n")



  ### Data source ------

  my_text <- paste0(my_text, "\n")  # add more spaces between sections

  if (include_comments) {
    my_text <- paste0(my_text, "\n# data source\n")
  }

  # for the ds_generalization, print the labels that belong to each class
  if (decoding_params$DS_type == "ds_generalization") {


    train_labels_text <- "train_label_levels <- list("
    test_labels_text <- "test_label_levels <- list("

    for (iClass in 1:decoding_params$DS_gen_class_number){

      curr_class_train_labels <- eval(str2lang(paste0("decoding_params$DS_gen_train_label_levels_class_", iClass)))
      curr_class_test_labels <- eval(str2lang(paste0("decoding_params$DS_gen_test_label_levels_class_", iClass)))

      curr_class_train_labels_text <- "c("
      for (iTrain in seq_along(curr_class_train_labels)) {
        curr_class_train_labels_text <- paste0(curr_class_train_labels_text,
                                               "'", curr_class_train_labels[iTrain], "', ")
      }

      curr_class_train_labels_text <- substr(curr_class_train_labels_text, 1, nchar(curr_class_train_labels_text) - 2)
      curr_class_train_labels_text <- paste0(curr_class_train_labels_text, "),")

      train_labels_text <- paste0(train_labels_text, "\n\t ", curr_class_train_labels_text)


      curr_class_test_labels_text <- "c("
      for (iTest in seq_along(curr_class_test_labels)) {
        curr_class_test_labels_text <- paste0(curr_class_test_labels_text,
                                               "'", curr_class_test_labels[iTest], "', ")
      }

      curr_class_test_labels_text <- substr(curr_class_test_labels_text, 1, nchar(curr_class_test_labels_text) - 2)
      curr_class_test_labels_text <- paste0(curr_class_test_labels_text, "),")

      test_labels_text <- paste0(test_labels_text, "\n\t ", curr_class_test_labels_text)

      # remove the DS_gen_train_label_levels_class_ from the decoding_params
      eval(str2lang(paste0("decoding_params[['DS_gen_train_label_levels_class_", iClass, "']] <- NULL")))
      eval(str2lang(paste0("decoding_params[['DS_gen_test_label_levels_class_", iClass, "']] <- NULL")))

    }  # end for loop over the number of classes


    train_labels_text <- substr(train_labels_text, 1, nchar(train_labels_text) - 1)
    train_labels_text <- paste0(train_labels_text, ")")

    test_labels_text <- substr(test_labels_text, 1, nchar(test_labels_text) - 1)
    test_labels_text <- paste0(test_labels_text, ")")

    decoding_params$DS_gen_class_number <- NULL

    my_text <- paste0(my_text, train_labels_text, "\n\n")
    my_text <- paste0(my_text, test_labels_text, "\n\n")


  } # end creating the training and test list for the ds_generalization



  my_text <- paste0(my_text, "ds <- ", decoding_params$DS_type,"(\n",
                    "\tbinned_data = binned_data,\n")

  for(element in names(decoding_params)){

    if(startsWith(element, "DS_basic_")){

      val <- eval(str2lang(paste0("decoding_params$",element)))

      # put the name of the variable to be decoding in quotes
      if (element == "DS_basic_var_to_decode") {
        val <- paste0("'", val, "'")
      }

      my_text <- paste0(my_text, "\t", gsub("DS_basic_", "", element)," = ", val, ",\n")
    }


    if(startsWith(element, "DS_gen_")){
      val <- eval(str2lang(paste0("decoding_params$",element)))
      my_text = paste0(my_text, "\t", gsub("DS_gen_", "", element)," = ", val, ",\n")
    }

  }

  if (decoding_params$DS_type == "ds_generalization") {

    my_text <- paste0(substring(my_text, 1, nchar(my_text)-1),
                      "\n\t", "train_label_levels = train_label_levels,",
                      "\n\t", "test_label_levels = test_label_levels) \n\n")

  } else {
    my_text <- paste0(substring(my_text, 1, nchar(my_text)-2), ") \n\n")
  }






  ### Classifier

  my_text <- paste0(my_text, "\n")  # add more spaces between sections

  if (include_comments) {
    my_text <- paste0(my_text, "\n# classifier\n")
  }

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



  ###  Feature Preprocessors

  my_text <- paste0(my_text, "\n")  # add more spaces between sections

  if (include_comments) {
    my_text <- paste0(my_text, "\n# feature preprocessors\n")
  }


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
      fp_skf = paste0(fp_skf, "\n\t", gsub("FP_skf_", "", element)," = ", val, ",")
    }
  }


  if(!is.null(fp_skf)){
    fp_skf = paste0(substring(fp_skf,1, nchar(fp_skf)-2), ") \n\n")
    fp_list = paste0(fp_list,"fp_skf,")
  }

  my_text = paste0(my_text,fp_skf,fp_zs)
  fp_list = paste0(substring(fp_list,1, nchar(fp_list)-1), ") \n\n")
  my_text = paste0(my_text, "fps <- list(",fp_list)





  ### Result metrics

  my_text <- paste0(my_text, "\n")  # add more spaces between sections

  if (include_comments) {
    my_text <- paste0(my_text, "\n# result metrics\n")
  }


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







  ###  Cross Validator

  my_text <- paste0(my_text, "\n")  # add more spaces between sections

  if (include_comments) {
    my_text <- paste0(my_text, "\n# cross validator\n")
  }


  my_text = paste0(my_text, "cv <- cv_standard(\n\t datasource = ds, \n")
  my_text = paste0(my_text,"\t classifier = cl, \n\t feature_preprocessors = fps, \n")
  for(element in names(decoding_params)){
    if(startsWith(element, "CV_")){
      val <- eval(str2lang(paste0("decoding_params$",element)))
      my_text = paste0(my_text, "\t ", gsub("CV_", "", element)," = ", val, ",\n")
    }
  }

  my_text <- paste0(substring(my_text,1, nchar(my_text)-2), ") \n\n")







  ### Run the decoding analysis

  my_text <- paste0(my_text, "\n")  # add more spaces between sections


  if (include_comments) {
    my_text <- paste0(my_text, "\n# run decoding analysis\n")
  }


  my_text <- paste0(my_text, "DECODING_RESULTS <- run_decoding(cv)\n\n")



  # print the analysis ID from running the decoding

  my_text <- paste0(my_text, "\n")  # add more spaces between sections

  if (include_comments) {
    my_text <- paste0(my_text, "\n# print out the ID for this analysis\n")
  }

  my_text <- paste0(my_text, "paste('The analysis ID is:',
        DECODING_RESULTS$cross_validation_paramaters$analysis_ID)\n\n")




  ### Save the results

  my_text <- paste0(my_text, "\n")  # add more spaces between sections


  if (include_comments) {
    my_text <- paste0(my_text, "\n# save the results\n")
  }


  results_save_directory <- decoding_params$results_save_dir


  my_text <- paste0(my_text, "log_save_results(DECODING_RESULTS, \n\t",
                    "'", results_save_directory, trimws(file.path(" ")), "')\n\n")


  my_text


}









generate_r_markdown_from_shiny_decoding_params <- function(decoding_params) {


  code_body <- generate_r_script_from_shiny_decoding_params(decoding_params)


  # fix the path to the data so that it is relative to where the R Markdown
  #   scripts are saved
  #code_body <- stringr::str_replace(code_body, "./data", "../../data")


  # fix the path to the results so that it is relative to where the R Markdown
  #   scripts are saved
  #code_body <- stringr::str_replace(code_body, "./results/", "../")


  my_text <- ""

  my_text <- paste0(my_text, "---\ntitle: 'Decoding Analysis'\noutput: pdf_document\n---\n\n\n",
                   "```{r setup, include=FALSE}\n\n\n",
                   "knitr::opts_chunk$set(echo = TRUE)\n\n\n",
                   "```\n\n\n")


  # my_text <- paste0(my_text, "\n\n\n```{r}\n\n\n")

  my_text <- paste0(my_text, "\n\n\n# Run the decoding analysis \n\n\n```{r}\n\n")


  my_text <- paste0(my_text, code_body)


  my_text <- paste0(my_text, "\n```\n\n\n")




  # can add some plots of results to the R Markdown file

  # This should be added to the UI
  add_plots_of_results <- TRUE


  if (add_plots_of_results) {

    my_text <- paste0(my_text, "\n\n\n# Plot some results \n\n\n```{r}\n")



    if ("rm_main_results"  %in% decoding_params$RM_type) {


      if (decoding_params$include_comments) {
        my_text <- paste0(my_text, "\n# plot main results")
      }

      my_text <- paste0(my_text, "\nplot(DECODING_RESULTS$rm_main_results, type = 'line') \n")


      if (decoding_params$include_comments) {
        my_text <- paste0(my_text, "\n# plot temporal-cross-decoding results")
      }

      my_text <- paste0(my_text, "\nplot(DECODING_RESULTS$rm_main_results) \n")

    }



    if ("rm_confusion_matrix"  %in% decoding_params$RM_type) {


      if (decoding_params$include_comments) {
        my_text <- paste0(my_text, "\n# plot confusion matrices")
      }

      my_text <- paste0(my_text, "\nplot(DECODING_RESULTS$rm_confusion_matrix) \n")


    }


    my_text <- paste0(my_text, "\n```\n\n\n")

  }





  my_text


}











