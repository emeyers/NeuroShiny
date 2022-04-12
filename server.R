


function(input, output, session){

  rv <- reactiveValues()

  rv$state_base_dir <- state_base_dir
  rv$state_cur_file_name <- ""

  rv$raster_base_dir <- raster_base_dir
  rv$raster_cur_dir_name <- NA
  rv$raster_cur_neuron <- 1
  rv$raster_num_neuron <- NA
  rv$raster_cur_file_name <- NULL
  rv$mRaster_cur_data <- NULL
  rv$raster_bRda <- FALSE
  rv$raster_bMat <-FALSE


  rv$create_bin_function_run <- ""
  rv$create_raster_function_run <- ""

  rv$binned_base_dir <- binned_base_dir
  rv$binned_file_name <- NA
  rv$binned_data <- NULL
  rv$binned_maximum_num_of_levels_in_all_var <- NULL
  rv$binned_all_var <- NULL

  rv$script_base_dir <- script_base_dir
  rv$script_chosen <- "No script chosen yet"
  rv$displayed_script <- ""

  rv$result_base_dir <- result_base_dir
  rv$result_chosen <- NA
  rv$result_data <- NULL


  rv$script_rmd_not_saved_yet <- 1

  rv$www_base_dir <- www_base_dir
  # only files meet specified files types will be shown. However, such dir shown as empty can still be choosed

  shinyFiles::shinyFileChoose(input, "home_loaded_state", roots = c(wd=state_base_dir), filetypes = "Rda")
  shinyFiles::shinyDirChoose(input, "bin_chosen_raster", roots = c(wd=raster_base_dir), filetypes = c("mat", "Rda"))
  shinyFiles::shinyFileChoose(input, "DS_binned_data", roots = c(wd=binned_base_dir), filetypes = "Rda")
  shinyFiles::shinyFileChoose(input, "DC_chosen_script_name", root =c(wd=script_base_dir, filetypes = c("R", "Rmd")))
  shinyFiles::shinyFileChoose(input, "Plot_chosen_result", root =c(wd=result_base_dir), filetypes = "Rda")








  observe({
    req(input$DS_binned_data)
    temp_df_file <- shinyFiles::parseFilePaths(c(wd= rv$binned_base_dir),input$DS_binned_data)
    req(temp_df_file$datapath)
    rv$binned_file_name <- temp_df_file$datapath

    load(rv$binned_file_name)
    rv$binned_data <- binned_data
    rv$binned_maximum_num_of_levels_in_all_var <-
      max(apply(select(binned_data, starts_with("labels"))[,],2, function(x) length(levels(as.factor(x)))))
    rv$binned_all_var <- sub("labels.", "", names(select(binned_data, starts_with("labels"))))
  })

  observe({
    req(input$DC_chosen_script_name)
    temp_df_file <- shinyFiles::parseFilePaths(c(wd= rv$script_base_dir),input$DC_chosen_script_name)
    req(temp_df_file$datapath)
    rv$script_chosen <- temp_df_file$datapath
    rv$displayed_script <- readChar(rv$script_chosen, file.info(rv$script_chosen)$size)
    updateTextInput(session, "DC_chosen_script_name", value = rv$script_chosen)
  })

  # when unzip a file, the new file is unzipped to exdir with origianl name, thus there is no need to update input with chosen file name




  observe({

    req(input$DC_to_be_saved_result_name)
    if(input$DC_script_mode == "R Markdown"){
      updateTextInput(session, "DC_to_be_saved_script_name", value = paste0(substr(input$DC_to_be_saved_result_name, 1,nchar(input$DC_to_be_saved_result_name)-3), "Rmd"))
    } else{
      updateTextInput(session, "DC_to_be_saved_script_name", value = paste0(substr(input$DC_to_be_saved_result_name, 1,nchar(input$DC_to_be_saved_result_name)-3), "R"))
    }
  })





  observeEvent(input$bin_save_raster_to_disk, {
    req(input$bin_uploaded_raster,input$bin_uploaded_raster_name )
    unzip(input$bin_uploaded_raster$datapath, exdir=input$bin_uploaded_raster_name)
  })


  observe({
    req(input$DS_uploaded_binned)
    temp_file_name <-input$DS_uploaded_binned$datapath
    updateTextInput(session, "DS_uploaded_binned_name", value = file.path(rv$binned_base_dir, basename(temp_file_name)))
  })

  observeEvent(input$DS_save_binned_to_disk, {
    req(input$DS_uploaded_binned,input$DS_uploaded_binned_name )
    move_file(input$DS_uploaded_binned$datapath,input$DS_uploaded_binned_name )


  })








  ### Bin the data ---------------------

  rv_para <- reactiveValues()

  # decoding_para_id changes. This is used by observerEvent who figures out the ids to signal eventReactive to check if they are in position
  rv_para$decoding_para_id_computed <- 1



  observeEvent(input$DC_scriptize,{
    rv_para$id <-  c("DS_binned_data", "DS_type", "DC_to_be_saved_result_name")
    if(input$DS_type == "ds_basic"){
      rv_para$id <- c(rv_para$id,"DS_var_to_decode")
      if(!input$DS_use_all_levels){
        rv_para$id <- c(rv_para$id,  "label_levels_to_use")
      }
    } else{
      rv_para$id <- c(rv_para$id,"DS_gen_var_to_use","DS_var_to_decode", "DS_gen_num_training_level_groups")
    }

    rv_para$inputID <- paste0("input$", rv_para$id)

    rv_para$decoding_para_id_computed <- rv_para$decoding_para_id_computed * (-1)
    eval(parse(text = paste0("req(", rv_para$inputID, ")")))
    ds_basic_paras <- gen_DS_paras <- NULL

    if(input$DS_type == "ds_basic"){
      ds_basic_paras <- c("label_levels_to_use")
    }else{
      gen_DS_paras <- c("train_label_levels", "label_levels")
    }


    DS_decoding <- paste0("DS_", c(all_DS_paras, ds_basic_paras, gen_DS_paras))

    rv_para$id_of_useful_paras <-  c("DS_type", DS_decoding)

    # this one is bad because level_groups can be passed from the previous selection
    if(input$DS_type == "ds_generalization"){
      temp_training_level_groups <- paste0("input$DS_training_level_group_", c(1:input$DS_gen_num_training_level_groups))
      temp_testing_level_groups <- paste0("input$DS_testing_level_group_", c(1:input$DS_gen_num_testing_level_groups))
      rv_para$id_of_useful_paras <- c(rv_para$id_of_useful_paras, trainin_level_groups, testing_level_groups)
    }




    if(input$DS_type == "ds_basic"){
      ds_basic_paras <- c("label_levels_to_use")
      rv_para$id_of_useful_paras <- c(rv_para$id_of_useful_paras,"DS_label_levels_to_use2")

    }else{
      gen_DS_paras <- c("DS_train_label_levels", "DS_test_label_levels")
      rv_para$id_of_useful_paras <- c(rv_para$id_of_useful_paras,"DS_train_label_levels", "DS_test_label_levels")
    }

    DS_decoding <- paste0("DS_", c(all_DS_paras, ds_basic_paras, gen_DS_paras))

    rv_para$inputID_of_useful_paras <- paste0("input$", DS_decoding)
    rv_para$values <- lapply(rv_para$inputID_of_useful_paras, function(i){
      eval(parse(text = i))
    })


    lDecoding_paras <- c(eval(rv$binned_file_name), input$DS_type, rv_para$values, input$DS_use_all_levels)

    lDecoding_paras <- setNames(lDecoding_paras, c("binned_data", rv_para$id_of_useful_paras))


    if (input$DC_script_mode == "R") {
      rv$displayed_script <- create_script_in_r(lDecoding_paras)
    }

  })

  er_scriptize_action_error <- eventReactive(rv_para$decoding_para_id_computed,{
    #if we don't have this line, this function will be called as soon as users click the script tab because rv_para$decoding_para_id_computed is going from NULL to 1 (I think)
    req(rv_para$id)
    validate(
      need(input$DS_binned_data, "No data has been uploaded")
    )
    temp_need = lapply(rv_para$id, function(i){
      #eval(parse(text = paste0("need(input$", i, ", '", "You need to set ",eval(parse(text = paste0("lLabels$", i))), "')")))
      eval(parse(text = paste0("need(input$", i, ", '", "You need to set it')")))

    })
    do.call(validate, temp_need)
  })

  output$DC_scriptize_error <- renderText({
    #er_scriptize_action_error()
    print("Mini Shiny Test")

  })




  #reactive_all_levels_of_basic_var_to_decode <- reactive({
  #  req(rv$binned_file_name)
  #  binned_data = rv$binned_data
  #  print("basic test")
  #  print(levels(factor(binned_data[[paste0("labels.",input$DS_var_to_decode)]])))
  #  levels(factor(binned_data[[paste0("labels.",input$DS_var_to_decode)]]))
  #})

  reactive_all_levels_of_basic_var_to_decode <- reactive({
    req(rv$binned_file_name)
    binned_data = rv$binned_data
    levels(factor(binned_data[[paste0("labels.",input$DS_basic_var_to_decode)]]))
  })


  reactive_all_levels_of_gen_train_var_to_use <- reactive({
    req(rv$binned_file_name)
    binned_data = rv$binned_data
    levels(factor(binned_data[[paste0("labels.",input$DS_gen_var_to_decode)]]))
  })

  reactive_all_levels_of_gen_test_var_to_use <- reactive({
    req(rv$binned_file_name)
    req(input$DS_gen_train_label_levels)

    binned_data = rv$binned_data
    tempLevels <- levels(factor(binned_data[[paste0("labels.",input$DS_gen_var_to_decode)]]))
    tempLevels[-(which(tempLevels %in% input$DS_gen_train_label_levels))]
  })



  output$DC_offer_scriptize = renderUI({
    list(
      textInput("DC_to_be_saved_result_name",
                "File name of the result to be saved (e.g., my_results)"),
      actionButton("DC_scriptize", "Generate script"),
      uiOutput("DC_scriptize_error")
    )
  })


## Data Source ----

  output$DS_show_chosen_bin = renderText({
    if(is.na(rv$binned_file_name)){
      "No file chosen yet"
    }else{
      basename(rv$binned_file_name)
    }
  })


#### Reactive Functions -----

  reactive_all_basic_site_IDs_to_use <- reactive({
    req(rv$binned_file_name)
    binned_data = rv$binned_data
    levels(unique(factor(binned_data$siteID)))
  })

  reactive_all_basic_site_IDs_to_exclude <- reactive({
    req(rv$binned_file_name)
    req(input$DS_basic_site_IDs_to_use)

    binned_data = rv$binned_data
    tempLevels <- levels(unique(factor(binned_data$siteID)))
    tempLevels[-(which(tempLevels %in% input$DS_basic_site_IDs_to_use))]
  })


  reactive_all_gen_site_IDs_to_use <- reactive({
    req(rv$binned_file_name)
    binned_data = rv$binned_data
    levels(unique(factor(binned_data$siteID)))
  })

  reactive_all_gen_site_IDs_to_exclude <- reactive({
    req(rv$binned_file_name)
    req(input$DS_gen_site_IDs_to_use)

    binned_data = rv$binned_data
    tempLevels <- levels(unique(factor(binned_data$siteID)))
    tempLevels[-(which(tempLevels %in% input$DS_gen_site_IDs_to_use))]
  })



  reactive_DS_levels_to_use <- reactive({
    req(rv$binned_data)
    if(input$DS_type == "ds_basic"){
      validate(
        need(!is.null(input$DS_basic_levels_to_use)||!input$DS_basic_use_all_levels, paste0("You haven't set ",
                                                                                            "your levels yet")))
                                                                                     #lLabels$DS_basic_levels_to_use, " yet")))
      if(!input$DS_basic_use_all_levels){
        reactive_all_levels_of_basic_var_to_decode()
      }else{
        input$DS_basic_levels_to_use
      }
    }else{
      #TO DO fix
      input$DS_gen_train_label_levels
      #reactive_all_levels_of_gen_var_to_use()
      #temp_training_level_group_ids <- paste0("input$DS_training_level_group_", c(1:3))
      #temp_training_level_group_ids <- paste0("input$DS_training_level_group_", c(1:input$DS_gen_num_training_level_groups))
      #temp_need <- lapply(temp_training_level_group_ids, function(i){
        #eval(parse(text = paste0("need(", i, ", '", "You need to set ", eval(parse(text = paste0("lLabels$", i))), "')")))
       # eval(parse(text = paste0("need(", i, ", '", "You need to set it')")))

      }
      #do.call(validate, temp_need)

      #temp_training_level_groups <- lapply(temp_training_level_group_ids, function(i){
      #  eval(parse(text = i))
      #})
      #unlist(temp_training_level_groups)

  })

  reactive_level_repetition_info <- reactive({
    req(reactive_DS_levels_to_use())
    if(input$DS_type == "ds_basic"){
      num_label_reps <- NeuroDecodeR::get_num_label_repetitions(rv$binned_data,
                                                                input$DS_basic_var_to_decode,
                                                                levels_to_use = reactive_DS_levels_to_use())
    }else{
      num_label_reps <- NeuroDecodeR::get_num_label_repetitions(rv$binned_data,
                                                                input$DS_gen_var_to_use,
                                                                levels_to_use = reactive_DS_levels_to_use())
    }
    print(num_label_reps)
    num_label_reps
  })

#### Basic Outputs ----

  output$DS_basic_list_of_var_to_decode = renderUI({
    req(rv$binned_file_name)
    selectInput("DS_basic_var_to_decode",
                "Variable to decode and to use",
                rv$binned_all_var)
  })

  output$DS_basic_list_of_levels_to_use = renderUI({
    selectInput("DS_basic_levels_to_use",
                "Levels to use",
                reactive_all_levels_of_basic_var_to_decode(),
                multiple = TRUE)
  })

  output$DS_basic_use_count_data = renderUI({
    selectInput("DS_basic_use_count_data",
                "Convert the data into spike counts",
                c(FALSE, TRUE))
  })


  output$DS_basic_site_IDs_to_use = renderUI({
    selectInput("DS_basic_site_IDs_to_use",
                "Which sites should be used",
                reactive_all_basic_site_IDs_to_use(),
                multiple = TRUE)
  })

  output$DS_basic_site_IDs_to_exclude = renderUI({
    selectInput("DS_basic_site_IDs_to_exclude",
                "Which sites should be excluded",
                reactive_all_basic_site_IDs_to_exclude(),
                multiple = TRUE)
  })

  output$DS_basic_randomly_shuffled_labels = renderUI({
    selectInput("DS_basic_randomly_shuffled_labels",
                "Randomly shuffle labels",
                c(FALSE, TRUE))
  })

  output$DS_basic_create_simultaneous_populations = renderUI({
    selectInput("DS_basic_create_simultaneous_populations",
                 "Was the data created simultaneously?",
                 c(0,1))
  })


#### General Output ----

  output$DS_gen_list_of_var_to_decode = renderUI({
    req(rv$binned_file_name)
    selectInput("DS_gen_var_to_decode",
                "Variable to decode and to use",
                rv$binned_all_var)
  })

  output$DS_gen_train_label_levels = renderUI({
    req(rv$binned_file_name)
    selectInput("DS_gen_train_label_levels",
                "Training levels to use",
                reactive_all_levels_of_gen_train_var_to_use(),
                multiple = TRUE)
  })

  output$DS_gen_test_label_levels = renderUI({
    req(rv$binned_file_name)
    selectInput("DS_gen_test_label_levels",
                "Testing labels to use",
                reactive_all_levels_of_gen_test_var_to_use(),
                multiple = TRUE)
  })

  output$DS_gen_use_count_data = renderUI({
    selectInput("DS_gen_use_count_data",
                "Convert the data into spike counts",
                c(FALSE, TRUE))
  })


  output$DS_gen_site_IDs_to_use = renderUI({
    selectInput("DS_gen_site_IDs_to_use",
                "Which sites should be used",
                reactive_all_gen_site_IDs_to_use(),
                multiple = TRUE)
  })

  output$DS_gen_site_IDs_to_exclude = renderUI({
    selectInput("DS_gen_site_IDs_to_exclude",
                "Which sites should be excluded",
                reactive_all_gen_site_IDs_to_exclude(),
                multiple = TRUE)
  })


  output$DS_gen_randomly_shuffled_labels = renderUI({
    selectInput("DS_gen_randomly_shuffled_labels",
                "Randomly shuffle labels",
                c(FALSE, TRUE))
  })

  output$DS_gen_create_simultaneous_populations = renderUI({
    selectInput("DS_basic_create_simultaneous_populations",
                "Was the data created simultaneously?",
                c(0,1))
  })


  #output$DS_gen_list_of_var_to_use = renderUI({
  #  req(rv$binned_file_name)
  #  selectInput("DS_gen_var_to_use",
  #              lLabels$DS_gen_var_to_use,
  #              rv$binned_all_var)
  #})

  #output$DS_gen_select_num_of_groups = renderUI({
  #  req(rv$binned_file_name)
  #  temp_max <- rv$binned_maximum_num_of_levels_in_all_var
  #  numericInput("DS_gen_num_training_level_groups",
  #               lLabels$DS_gen_num_training_level_groups,
  #               1,
  #               min = 1,
  #               max  = temp_max)
  #})

  #output$DS_gen_list_of_training_level_groups = renderUI({
  #  req(input$DS_gen_num_training_level_groups)
  #  temp_num <- input$DS_gen_num_training_level_groups
  #  temp_output <- lapply(1:temp_num, function(i){
  #    list(selectInput(paste0("DS_training_level_group_", i),
  #                     paste("Training level group", i),
  #                     reactive_all_levels_of_gen_var_to_use(),
  #                     multiple = TRUE
  #    ),
  #    selectInput(paste0("DS_testing_level_group_", i),
  #                paste("Testing level group", i),
  #                reactive_all_levels_of_gen_var_to_use(),
  #                multiple = TRUE
  #    ))
  #  })
  #  temp_output <- unlist(temp_output, recursive = FALSE)
  #  temp_output
  #})


  ### Cross Valid Temp ----

  #### Reactive Functions ----
  #TODO: max_repetition_avail_with_any_site?? maybe change to DS_max_repetition_avail_with_any_site
  observe({
    req(reactive_level_repetition_info())
    temp_level_rep_info <- reactive_level_repetition_info()
    if(input$DS_type == "ds_basic"){
      updateNumericInput(session, "DS_basic_num_label_repeats_per_cv_split",
                         max = floor(temp_level_rep_info$DS_max_repetition_avail_with_any_site/input$DS_basic_num_cv_splits))
      updateNumericInput(session, "DS_basic_num_cv_splits",
                         max = floor(temp_level_rep_info$DS_max_repetition_avail_with_any_site/input$DS_basic_num_label_repeats_per_cv_split))
    }else{
      updateNumericInput(session, "DS_gen_num_label_repeats_per_cv_splits",
                         max = floor(temp_level_rep_info$DS_max_repetition_avail_with_any_site/input$DS_gen_num_cv_splits))
      updateNumericInput(session, "DS_gen_num_cv_splits",
                         max = floor(temp_level_rep_info$DS_max_repetition_avail_with_any_site/input$DS_gen_num_label_repeats_per_cv_splits))
    }
  })

  reactive_level_repetition_info <- reactive({
    req(reactive_DS_levels_to_use())
    if(input$DS_type == "ds_basic"){
      num_label_reps <- NeuroDecodeR::get_num_label_repetitions(rv$binned_data,
                                                                input$DS_basic_var_to_decode,
                                                                levels_to_use = reactive_DS_levels_to_use())
    }else{
      #TO DO what is this for gen?
      num_label_reps <- NeuroDecodeR::get_num_label_repetitions(rv$binned_data,
                                                                input$DS_gen_var_to_decode,
                                                                levels_to_use = reactive_DS_levels_to_use())
    }
    num_label_reps
  })


  reactive_chosen_repetition_info <- reactive({
    if(input$DS_type == "ds_basic"){
      req(input$DS_basic_num_cv_splits, input$DS_basic_num_label_repeats_per_cv_split, reactive_level_repetition_info())
      temp_level_repetition_info <- reactive_level_repetition_info()
      list(num_repetition = input$DS_basic_num_label_repeats_per_cv_split * input$DS_basic_num_cv_splits,
      num_sites_avail = nrow(filter(temp_level_repetition_info, min_repeats >= input$DS_basic_num_label_repeats_per_cv_split * input$DS_basic_num_cv_splits)))

    }else{
      req(input$DS_gen_num_cv_splits, input$DS_gen_num_label_repeats_per_cv_split, reactive_level_repetition_info())
      temp_level_repetition_info <- reactive_level_repetition_info()
      list(num_repetition = input$DS_gen_num_label_repeats_per_cv_split * input$DS_gen_num_cv_splits,
           num_sites_avail = nrow(filter(temp_level_repetition_info, min_repeats >= input$DS_gen_num_label_repeats_per_cv_split * input$DS_gen_num_cv_splits)))
    }
  })


  #### General Outputs ----

  output$DS_max_repetition_avail_with_any_site <- renderText({
    req(reactive_level_repetition_info())
    temp_level_rep_info <- reactive_level_repetition_info()
    paste("Levels chosen for training:", "<font color='red'>",
          paste(reactive_DS_levels_to_use(), collapse = ', '),
          "<br/>", "</font>", "The maximum number of repetitions across all the levels for training as set on the Data Source tab is",
          "<font color='red'>",
          min(temp_level_rep_info$min_repeats), "</font>", ".")
  })

  output$DS_show_chosen_repetition_info <- renderText({
    req(reactive_chosen_repetition_info())
    temp_chosen_repetition_info <- reactive_chosen_repetition_info()
    if (input$DS_type == "ds_basic"){
      paste("You selected", "<font color='red'>",
            temp_chosen_repetition_info$num_repetition, "</font>",
            "trials (", input$DS_basic_num_label_repeats_per_cv_split,
            " repeats x ",  input$DS_basic_num_cv_splits,
            "CV splits). Based on the levels selected Data source tab, this gives <font color='red'>")
      #,temp_chosen_repetition_info$num_sites_avail, "</font>", " sites available for decoding.")
    }else{
      paste("You selected", "<font color='red'>",
            temp_chosen_repetition_info$num_repetition, "</font>",
            "trials (", input$DS_gen_num_label_repeats_per_cv_split,
            " repeats x ",  input$DS_gen_num_cv_splits,
            "CV splits). Based on the levels selected Data source tab, this gives <font color='red'>")
      #,temp_chosen_repetition_info$num_sites_avail, "</font>", " sites available for decoding.")
    }

  })

  #### Basic Outputs ----

  output$DS_basic_num_label_repeats_per_cv_split = renderUI({
    numericInput("DS_basic_num_label_repeats_per_cv_split",
                 "Number of repeats of each level in each CV split",
                 value = 1, min = 2)
  })

  output$DS_basic_num_cv_splits = renderUI({
    req(rv$binned_file_name)
    numericInput("DS_basic_num_cv_splits",
                 "Number of cross validation splits",
                 value = 2, min = 2)

  })

  output$DS_basic_num_resample_sites = renderUI({
    numericInput("DS_basic_num_resample_sites",
                 "Number of resampling sites",
                 value = NULL, min = 1)

  })


  #### General Outputs ----

  output$DS_gen_num_label_repeats_per_cv_split = renderUI({
    numericInput("DS_gen_num_label_repeats_per_cv_split",
                 "Number of repeats of each level in each CV split",
                 value = 1, min = 2)
  })

  output$DS_gen_num_cv_splits = renderUI({
    req(rv$binned_file_name)
    numericInput("DS_gen_num_cv_splits",
                 "Number of cross validation splits",
                 value = 2, min = 2)

  })

  output$DS_gen_num_resample_sites = renderUI({
    numericInput("DS_gen_num_resample_sites",
                 "Number of resampling sites",
                 value = NULL, min = 1)

  })

  #### Plot ----
  output$DS_show_level_repetition_info <- renderPlotly({
    req(reactive_level_repetition_info())
    temp_level_repetition_info <- reactive_level_repetition_info()
    ggplotly(plot(temp_level_repetition_info))

  })


  ### Feature Processing ----
  #### Reactive Functions ----
  reactive_all_fp_avail <- reactive({
    req(input$CL)
    all_fp[df_cl_fp[,input$CL]>0]
  })

  reactive_bin_num_neuron <- reactive({
    validate(
      need(input$DS_binned_data,"Please select data source first to get total number of neurons")
    )
    binned_data = rv$binned_data
    length(unique(factor(binned_data$siteID)))
  })

  #### Output ----
  output$FP_type = renderUI({
    checkboxGroupInput("FP_type",
                       "Feature Preprocessors",
                       reactive_all_fp_avail())
  })

  output$FP_num_site_to_use = renderUI({
    req(input$FP_type)
    if("fp_select_k_features" %in% input$FP_type){
      numericInput("FP_num_site_to_use",
                   "Select top features? (this will be applied first)",
                   reactive_bin_num_neuron(),
                   min = 1,
                   max = reactive_bin_num_neuron())
    }
  })

  output$FP_num_sites_to_exclude = renderUI({
    req(input$FP_num_site_to_use)
    if("fp_select_k_features" %in% input$FP_type){
      numericInput("FP_num_sites_to_exclude",
                   "exclude top ? features (this will be applied second)",
                   value = 0,
                   min = 1,
                   max = reactive_bin_num_neuron() - input$FP_num_site_to_use)
    }
  })


  ### Cross-Validator ----
  #### Output ----
  output$CV_num_resample_runs = renderUI({
    numericInput("CV_num_resample_runs",
                 "Number of resampling runs",
                 value = 50, min = 1)

  })

  output$CV_num_parallel_cores = renderUI({
    numericInput("CV_num_parallel_cores",
                 "Number of parallel cores for resample runs",
                 value = NULL)
  })

  output$CV_parallel_outfile = renderUI({
    req(input$CV_advanced == TRUE)
    req(!is.null(input$CV_num_parallel_cores))
    req(input$CV_num_parallel_cores >= 1)

    textInput("CV_parallel_outfile",
                 "File name of output from parallel cores",
                 value = NULL)
  })



  ### Run Decoding ----

  output$DC_ace = renderUI({
    shinyAce::aceEditor("script",
                        rv$displayed_script,
                        mode = input$DC_script_mode)
  })

  observeEvent(input$DC_run_decoding,{
    req(input$DC_to_be_saved_result_name, rv$displayed_script)
    # add the appropriate file extenstion to the saved file name
    if(input$DC_script_mode == "R Markdown"){
      file_extension <- ".Rmd"
    }else{
      file_extension <- ".R"
    }

    file_pieces <- unlist(base::strsplit(input$DC_to_be_saved_result_name, "[.]"))

    if(length(file_pieces) == 1){
      save_file_name <- paste0(file_pieces[1], file_extension)
    }else{
      if(!(file_pieces[length(file_pieces)] == file_extension)){
        save_file_name <- paste0(save_file_name, file_extension)
      }else{
        save_file_name <- input$DC_to_be_saved_result_name
      }
    }


    save_script_name <- file.path(script_base_dir, save_file_name)
    write(rv$displayed_script, file = save_script_name)

    rv$save_script_name <- save_script_name

    # run the script/Markdown document to get the results

    if(input$DC_script_mode == "R Markdown") {
      #save_file_name to www directory
      rmarkdown::render(save_script_name, "pdf_document", output_dir = "www")
    }else{
      source(save_script_name)
      }
    })


}
