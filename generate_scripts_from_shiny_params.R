

# Function converts the selected parameters to an R script
# Essentially creates a long string
generate_r_script_from_shiny_decoding_params <- function(decoding_params) {

  # Remove any fields that are null
  decoding_params <-  decoding_params[!is.na(decoding_params)]

  # Conditional variable to check for optional comments added to script
  include_comments <- decoding_params$include_comments

  # Load the NDR
  my_text <- "library(NeuroDecodeR)\n\n"

  # Binned Data
  if (include_comments) {
    my_text <- paste0(my_text, "\n# binned file name\n")
  }

  # Using the base directory, parse the file path and file name for
  # the binned data
  bin_data <- file.path(decoding_params$binned_dir_name,
                        decoding_params$DS___p___binned_data)

  my_text <- paste0(my_text, "binned_data <- file.path('",
                    bin_data, "') \n\n")


  # Data source
  my_text <- paste0(my_text, "\n")  # Add space between sections

  if (include_comments) {
    my_text <- paste0(my_text, "\n# data source\n")
  }

  # Creating ds_generalization training and testing labels, not added until later
  if (decoding_params$DS_type == "ds_generalization") {

    # Start of string for training and testing levels
    train_labels_text <- "train_label_levels <- list("
    test_labels_text <- "test_label_levels <- list("

    # Add the labels that belong to each class
    for (class_i in 1:decoding_params$DS_gen___np___class_number) {
      # Assign train and test labels for class i
      class_i <- as.character(class_i)
      class_i_train_labels <- eval(str2lang(paste0("decoding_params$DS_gen___p___train_label_levels_class_", class_i)))
      class_i_test_labels <- eval(str2lang(paste0("decoding_params$DS_gen___p___test_label_levels_class_", class_i)))

      # Start of string for class i's labels
      class_i_train_labels_text <- "c("
      class_i_test_labels_text <- "c("

      # Add training and testing labels to string
      for (train_i in seq_along(class_i_train_labels)) {
        class_i_train_labels_text <- paste0(class_i_train_labels_text, "'",
                                            class_i_train_labels[train_i], "', ")
      }
      for (test_i in seq_along(class_i_test_labels)) {
        class_i_test_labels_text <- paste0(class_i_test_labels_text, "'",
                                           class_i_test_labels[test_i], "', ")
      }

      # Remove last ", " in the string to close the list with "),
      class_i_train_labels_text <- gsub('.{2}$', "),", class_i_train_labels_text)
      class_i_test_labels_text <- gsub('.{2}$', "),", class_i_test_labels_text)

      # Add the new class label list to the full set lists for training labels
      train_labels_text <- paste0(train_labels_text, "\n\t ", class_i_train_labels_text)
      test_labels_text <- paste0(test_labels_text, "\n\t ", class_i_test_labels_text)

      # Remove the DS_gen___p___train_label_levels_class_ for class i from the decoding_params
      eval(str2lang(paste0("decoding_params[['DS_gen___p___train_label_levels_class_", class_i, "']] <- NULL")))
      eval(str2lang(paste0("decoding_params[['DS_gen___p___test_label_levels_class_", class_i, "']] <- NULL")))

    }  # End for loop over the number of classes


    # Remove final character to replace with ")" and close list
    train_labels_text <- gsub('.{1}$', ")", train_labels_text)
    test_labels_text <- gsub('.{1}$', ")", test_labels_text)

    # Remove the number of classes from decoding_params
    decoding_params$DS_gen___np___class_number <- NULL

    # Add space between sections
    my_text <- paste0(my_text, train_labels_text, "\n\n")
    my_text <- paste0(my_text, test_labels_text, "\n\n")


    # End creating the training and test list for the ds_generalization

  }



  # Add start of data source assignment
  my_text <- paste0(my_text, "ds <- ", decoding_params$DS_type,"(\n",
                    "\tbinned_data = binned_data,\n")

  for (element in names(decoding_params)) {

    # For ds_basic parameters
    if (startsWith(element, "DS_basic___p___")) {

      val <- eval(str2lang(paste0("decoding_params$",element)))

      # Put the name of the variable to be decoded in quotes
      if (element == "DS_basic___p___labels") {
        val <- paste0("'", val, "'")
      }

      # If label_levels have been specified, put them in as a vector argument
      if (element == "DS_basic___p___label_levels" ) {
        val <- paste0("c('", paste(val, collapse = "', '"), "')")
      }

      # For site IDs to be include/exclude, put them in as an int vector
      if (element == "DS_basic___p___site_IDs_to_use" ||
          element == "DS_basic___p___site_IDs_to_exclude") {
        val <- paste0("c(", paste(val, collapse = ", "), ")")
      }



      # Add current element and it's value to string
      my_text <- paste0(my_text, "\t", gsub("DS_basic___p___", "", element)," = ", val, ",\n")
    }


    if(startsWith(element, "DS_gen___p___")){
      val <- eval(str2lang(paste0("decoding_params$", element)))
      # Add all current existing ds_generalization elements in decoding_params
      my_text <- paste0(my_text, "\t", gsub("DS_gen___p___", "", element)," = ", val, ",\n")
    }

  }

  if (decoding_params$DS_type == "ds_generalization") {
    # Add training and testing levels from above, then finish data source
    my_text <- paste0(gsub('.{1}$', "", my_text),
                      "\n\t", "train_label_levels = train_label_levels,",
                      "\n\t", "test_label_levels = test_label_levels) \n\n")

  } else {
    # Otherwise, just finish data source
    my_text <- gsub('.{2}$', ") \n\n", my_text)
  }


  # Classifier

  my_text <- paste0(my_text, "\n")  # Add space between sections

  if (include_comments) {
    my_text <- paste0(my_text, "\n# classifier\n")
  }

  my_text <- paste0(my_text, "cl <- ", decoding_params$CL_type, "(")

  if ("CL___p___return_decision_values" %in% names(decoding_params)) {
    val <- eval(str2lang("decoding_params$CL___p___return_decision_values"))
    my_text <- paste0(my_text,"return_decision_values = ", val, ",\n")
  }

  for (element in names(decoding_params)) {

    # Add support vector machine classifier, if applicable
    if (startsWith(element, "CL_svm___p___")) {
      val <- eval(str2lang(paste0("decoding_params$",element)))
      my_text <- paste0(my_text, gsub("CL_svm___p___", "", element), " = ", val, ",\n")
    }
  }

  # Finish classifiers
  my_text <- gsub('.{2}$', ") \n\n", my_text)



  #  Feature Preprocessors ----

  my_text <- paste0(my_text, "\n")  # Add space between sections

  if (include_comments) {
    my_text <- paste0(my_text, "\n# feature preprocessors\n")
  }

  # Initialize empty strings
  fp_zs <- NULL
  fp_skf <- NULL
  fp_list <- NULL

  for (element in names(decoding_params)) {
    if (startsWith(element, "FP_type")) {
      # Add zscore feature preprocessor if selected
      if ("fp_zscore" %in% decoding_params$FP_type) {
        fp_zs <- "fp_zs <- fp_zscore()\n\n"
        fp_list <-  paste0(fp_list, "fp_zs,")
      }
      # Add select_k_features feature preprocessor if selected
      if ("fp_select_k_features" %in% decoding_params$FP_type) {
        fp_skf <- "fp_skf <- fp_select_k_features("
      }
    }
    # If select_k_features is selected, add additional parameters
    if (startsWith(element, "FP_skf___p___")) {
      val <- eval(str2lang(paste0("decoding_params$",element)))
      fp_skf <- paste0(fp_skf, "\n\t", gsub("FP_skf___p___", "", element)," = ", val, ",")
    }
  }
  if (!is.null(fp_skf)) {
    # Close select_k_features and add to string for all feature preprocessors
    fp_skf <- gsub('.{1}$', ") \n\n", fp_skf)
    fp_list <- paste0(fp_list,"fp_skf,")
  }

  my_text <- paste0(my_text, fp_skf, fp_zs)
  fp_list <- gsub('.{1}$', ") \n\n", fp_list)
  my_text <- paste0(my_text, "fps <- list(", fp_list)





  # Result metrics ----

  my_text <- paste0(my_text, "\n")  # Add space between sections

  if (include_comments) {
    my_text <- paste0(my_text, "\n# result metrics\n")
  }

  # Initialize empty strings
  rm_main_text <- NULL
  rm_cm_text <- NULL

  # Add selected result metrics
  if ("rm_main_results" %in% decoding_params$RM_type) {
    rm_main_text <- paste0("rm_main <- rm_main_results(\n\t include_norm_rank_results = ",
                           as.character(decoding_params$RM_mr___p___include_norm_rank_results), ")\n")
  }

  if ("rm_confusion_matrix" %in% decoding_params$RM_type) {
    rm_cm_text <- paste0("rm_cm <- rm_confusion_matrix(\n\t save_TCD_results = ",
                         as.character(decoding_params$RM_cm___p___save_TCD_results), ",\n\t ",
                         "create_decision_vals_confusion_matrix = ",
                         as.character(decoding_params$RM_cm___p___create_decision_vals_confusion_matrix), ")\n")
  }


  # Create text list of selected results metrics
  rm_list_text <- "rms <- list("
  if (!is.null(rm_main_text)) {
    rm_list_text <- paste0(rm_list_text, "rm_main")
  }
  if (!is.null(rm_cm_text)) {
    if (!is.null(rm_main_text)) {
      rm_list_text <- paste0(rm_list_text, ", ")
    }
    rm_list_text <- paste0(rm_list_text, "rm_cm")
  }
  rm_list_text <- paste0(rm_list_text, ")\n\n")


  my_text <- paste0(my_text, rm_main_text, rm_cm_text, rm_list_text)




  #  Cross Validator

  my_text <- paste0(my_text, "\n")  # Add space between sections

  if (include_comments) {
    my_text <- paste0(my_text, "\n# cross validator\n")
  }

  my_text <- paste0(my_text, "cv <- cv_standard(\n\t datasource = ds,\n")
  my_text <- paste0(my_text,"\t classifier = cl, \n\t feature_preprocessors = fps,\n")
  my_text <- paste0(my_text,"\t result_metrics = rms,\n")

  # Add existing cross validator standard parameters
  for (element in names(decoding_params)) {
    if (startsWith(element, "CV_standard___p___")) {
      val <- eval(str2lang(paste0("decoding_params$",element)))
      if (element == "CV_standard___p___parallel_outfile"){
        my_text <- paste0(my_text, "\t ", gsub("CV_standard___p___", "", element),
                          " = '", val, "',\n")
      } else {
        my_text <- paste0(my_text, "\t ", gsub("CV_standard___p___", "", element),
                          " = ", val, ",\n")
      }

    }
  }

  my_text <- gsub('.{2}$', ") \n\n", my_text)





  # Run the decoding analysis ----

  my_text <- paste0(my_text, "\n")  # Add space between sections


  if (include_comments) {
    my_text <- paste0(my_text, "\n# run decoding analysis\n")
  }


  my_text <- paste0(my_text, "DECODING_RESULTS <- run_decoding(cv)")
  my_text <- paste0(my_text, "\n\n")  # Add space between sections

  if (include_comments) {
    my_text <- paste0(my_text, "\n# print out the ID for this analysis\n")
  }

  # Print the analysis ID from running the decoding
  my_text <- paste0(my_text, "paste('The analysis ID is:',
        DECODING_RESULTS$cross_validation_paramaters$analysis_ID)\n\n")




  # Save the results ----

  my_text <- paste0(my_text, "\n")  # Add space between sections


  if (include_comments) {
    my_text <- paste0(my_text, "\n# save the results\n")
  }

  results_save_directory <- decoding_params$results_save_dir


  my_text <- paste0(my_text, "log_save_results(DECODING_RESULTS, \n\t",
                    "'", decoding_params$results_dir_name,"'")

  if(decoding_params$result_name != ""){
    my_text <- paste0(my_text, ",\n result_name = '",
                      decoding_params$result_name, "'")
  }

  my_text <- paste0(my_text,  ")\n\n")

  my_text

}









generate_r_markdown_from_shiny_decoding_params <- function(decoding_params) {

  # Generate the same code from R script
  code_body <- generate_r_script_from_shiny_decoding_params(decoding_params)

  include_comments <- decoding_params$include_comments

  my_text <- ""

  # Add markdown header
  my_text <- paste0(my_text, "---\ntitle: 'Decoding Analysis'\noutput: pdf_document\n---\n\n\n",
                    "```{r setup, include=FALSE}\n\n\n",
                    "knitr::opts_chunk$set(echo = TRUE)\n\n\n",
                    "```\n\n\n")

  # Start chunk
  my_text <- paste0(my_text, "\n\n\n# Run the decoding analysis \n\n\n```{r}\n\n")
  # Add code
  my_text <- paste0(my_text, code_body)
  # Close chunk
  my_text <- paste0(my_text, "\n```\n\n\n")



  # Start chunk
  my_text <- paste0(my_text, "\n\n\n# Plot some results \n\n\n```{r}\n")

  if ("rm_main_results" %in% decoding_params$RM_type) {
    if (include_comments) {
      my_text <- paste0(my_text, "\n# plot main results")
    }

    my_text <- paste0(my_text, "\nplot(DECODING_RESULTS$rm_main_results, type = 'line') \n")

    if (include_comments) {
      my_text <- paste0(my_text, "\n# plot temporal-cross-decoding results")
    }

    my_text <- paste0(my_text, "\nplot(DECODING_RESULTS$rm_main_results) \n")

  }

  if ("rm_confusion_matrix" %in% decoding_params$RM_type) {
    if (include_comments) {
      my_text <- paste0(my_text, "\n# plot confusion matrices")
    }

    my_text <- paste0(my_text, "\nplot(DECODING_RESULTS$rm_confusion_matrix) \n")
  }

  # Close chunk
  my_text <- paste0(my_text, "\n```\n\n\n")



  my_text

}


# To create script name in the run script/save and run script server action
generate_script_name <- function(script_mode, result_base_dir) {
  if (script_mode == "R") {
    script_save_dir <- "r_scripts"
    file_extension <- ".R"
  } else if (script_mode == "R Markdown") {
    script_save_dir <- "r_markdown"
    file_extension <- ".Rmd"
  }

  # generate analysis script name
  # TODO should perhaps do this when the script is generated and then can add the script name as meta
  # data to be saved with the decoding results, but ok for now...
  script_file_name <- paste0(trimws(file.path(result_base_dir, script_save_dir, " ")),
                             "NeuroShiny_Script_ID_",
                             NeuroDecodeR:::generate_analysis_ID(),
                             file_extension)
  return(script_file_name)

}

