


function(input, output, session){

  rv <- reactiveValues()

  rv$working_dir <- working_dir
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

  # only files meet specified files types will be shown. However, such dir shown as empty can still be choosen

  shinyFiles::shinyDirChoose(input, "bin_chosen_raster", roots = c(wd=raster_base_dir), filetypes = c("mat", "Rda"))
  shinyFiles::shinyFileChoose(input, "DS___p___binned_data", roots = c(wd=binned_base_dir), filetypes = "Rda")
  shinyFiles::shinyFileChoose(input, "Plot_chosen_result", root = c(wd=result_base_dir), filetypes = "Rda")





  observe({
    req(input$DS___p___binned_data)
    temp_df_file <- shinyFiles::parseFilePaths(c(wd= rv$binned_base_dir),input$DS___p___binned_data)
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
    temp_df_file <- shinyFiles::parseFilePaths(c(wd=rv$script_base_dir),input$DC_chosen_script_name)
    req(temp_df_file$datapath)
    rv$script_chosen <- temp_df_file$datapath
    rv$displayed_script <- readChar(rv$script_chosen, file.info(rv$script_chosen)$size)
    updateTextInput(session, "DC_chosen_script_name", value = rv$script_chosen)
  })

  # when unzip a file, the new file is unzipped to exdir with original name, thus there is no need to update input with chosen file name




  # observe({
  #   req(input$DC_to_be_saved_result_name)
  #   if(input$DC_script_mode == "R Markdown"){
  #     updateTextInput(session, "DC_to_be_saved_script_name", value = paste0(substr(input$DC_to_be_saved_result_name, 1,nchar(input$DC_to_be_saved_result_name)-3), "Rmd"))
  #   } else{
  #     updateTextInput(session, "DC_to_be_saved_script_name", value = paste0(substr(input$DC_to_be_saved_result_name, 1,nchar(input$DC_to_be_saved_result_name)-3), "R"))
  #   }
  # })



  observeEvent(input$bin_save_raster_to_disk, {
    req(input$bin_uploaded_raster,input$bin_uploaded_raster_name )
    unzip(input$bin_uploaded_raster$datapath, exdir=input$bin_uploaded_raster_name)
  })



  observe({
    req(input$DS_uploaded_binned)
    temp_file_name <-input$DS_uploaded_binned$datapath
    updateTextInput(session, "DS_uploaded_binned_name", value = file.path(rv$binned_base_dir, basename(temp_file_name)))
  })



  observeEvent(input$DS_save_binned_to_disk,{
    req(input$DS_uploaded_binned, input$DS_uploaded_binned_name )
    move_file(input$DS_uploaded_binned$datapath, input$DS_uploaded_binned_name)
  })


  output$home_offer_save_state = renderUI({
    list(
      textInput("home_state_name",
                "File name of the current state should be saved (e.g., state_01.Rda)",
                rv$working_dir),
      actionButton("home_save_state", "Save the current state"),
      uiOutput("home_save_state_error")
    )
  })

  output$home_save_state_error = renderUI({
    er_home_save_state_error()
  })

  er_home_save_state_error <- eventReactive(input$home_save_state, {
    validate(
      need(input$home_state_name, paste0("Requires name of the file to be saved"))
    )
  })

  observeEvent(input$home_save_state, {
    req(input$home_state_name)
    state = reactiveValuesToList(input)
    save(state, file = input$home_state_name)

  })



  observe({
    req(input$bin_chosen_raster)
    rv$raster_cur_dir_name <- shinyFiles::parseDirPath(c(wd= rv$raster_base_dir),input$bin_chosen_raster)
    req(rv$raster_cur_dir_name)
    mat_files_in_raster_dir <-
      list.files(rv$raster_cur_dir_name, pattern = "\\.mat$")

    if(length(mat_files_in_raster_dir) > 0){
      rv$raster_bMat <- TRUE
    }else{
      rv$raster_bMat <-FALSE
      rda_files_in_raster_dir <-
        list.files(rv$raster_cur_dir_name, pattern = "\\.[rR]da$")
      rv$raster_num_neuron <- length(rda_files_in_raster_dir)

      if(rv$raster_num_neuron > 0){
        rv$raster_bRda <- TRUE
        rv$raster_cur_file_name <- rda_files_in_raster_dir[rv$raster_cur_neuron]
        load(file.path(rv$raster_cur_dir_name, rv$raster_cur_file_name))
        temp_dfRaster <- select(raster_data, starts_with("time."))
        temp_mRaster <- as.matrix(temp_dfRaster)
        rownames(temp_mRaster) <- 1:dim(temp_mRaster)[1]
        colnames(temp_mRaster) <- gsub("time.", "", colnames(temp_mRaster))
        rv$mRaster_cur_data <- temp_mRaster
      }else{
        rv$raster_bRda <- FALSE
        # this doesn't work; observe is for action not calculation
        # validate("Only accept raster data in .mat or .Rda format !")
      }
    }
  })





  ### Bin the data ---------------------

  observeEvent(input$bin_bin_data,{
    if(rv$raster_bRda){

      # data binned data in the director data/binned
      #binned_basename <- trimws(file.path("data", "binned", " "))

      temp_call = paste0("NeuroDecodeR:::create_binned_data(rv$raster_cur_dir_name, ",
                         "paste0(binned_base_dir, input$bin_prefix_of_binned_file_name),",
                         "input$bin_bin_width, input$bin_step_size")

      if(!is.na(input$bin_start_ind)){
        temp_call = paste0(temp_call, ",input$bin_start_ind")
      }
      if(!is.na(input$bin_end_ind)){
        temp_call = paste0(temp_call, ",input$bin_end_ind")
      }
      temp_call = paste0(temp_call,")")
      #rv$create_bin_function_run <- temp_call

      rv$create_bin_function_run <- paste0("NeuroDecodeR:::create_binned_data('",
                                           rv$raster_cur_dir_name, "', ",
                                           #"'", binned_basename,
                                           "'", binned_base_dir,
                                           input$bin_prefix_of_binned_file_name, "', ",
                                           input$bin_bin_width, ", ", input$bin_step_size, ")")

      eval(parse(text = temp_call))

    } else if(rv$raster_bMat){

      temp_call = paste0("NeuroDecodeR:::create_binned_data_from_matlab_raster_data(rv$raster_cur_dir_name,",
                         "input$bin_prefix_of_binned_file_name,",
                         "input$bin_bin_width, input$bin_step_size")
      if(!is.na(input$bin_start_ind)){
        temp_call = paste0(temp_call, ",input$bin_start_ind")
      }
      if(!is.na(input$bin_end_ind)){
        temp_call = paste0(temp_call, ",input$bin_end_ind")
      }
      temp_call = paste0(temp_call,")")
      rv$create_bin_function_run <- temp_call
      eval(parse(text = temp_call))

    }

  })

  observeEvent(input$bin_create_raster,{

    temp_call = paste0("NeuroDecodeR:::create_raster_data_from_matlab_raster_data(rv$raster_cur_dir_name,",
                       "input$bin_new_raster")
    if(!is.na(input$bin_start_ind)){
      temp_call = paste0(temp_call, ",input$bin_raster_start_ind")
    }
    if(!is.na(input$bin_end_ind)){
      temp_call = paste0(temp_call, ",input$bin_raster_end_ind")
    }
    temp_call = paste0(temp_call,")")
    rv$create_raster_funciton_run <- temp_call
    eval(parse(text = temp_call))


  })

  rv_para <- reactiveValues()

  # decoding_para_id changes. This is used by observerEvent who figures out the ids to signal eventReactive to check if they are in position
  rv_para$decoding_para_id_computed <- 1


  observeEvent(input$bin_pre_neuron,{
    if(rv$raster_cur_neuron > 1){
      rv$raster_cur_neuron <- rv$raster_cur_neuron - 1
    }
  })

  observeEvent(input$bin_next_neuron,{
    if(rv$raster_cur_neuron < rv$raster_num_neuron){
      rv$raster_cur_neuron <- rv$raster_cur_neuron + 1
    }
  })

  reactive_bRaster_qualified <- reactive({
    sum(rv$raster_bMat, rv$raster_bRda)
  })

  output$bin_offer_create_raster = renderUI({
    req(rv$raster_cur_dir_name)
    if(rv$raster_bMat){
      temp_matlab_raster_dir_name <- rv$raster_cur_dir_name
      # if the directory name ends with _mat, remove _mat
      temp_non_desired_pattern = '.*_mat$'
      if (grepl(temp_non_desired_pattern, temp_matlab_raster_dir_name) == TRUE){
        temp_r_raster_dir_name <- substr(temp_matlab_raster_dir_name, 1, nchar(temp_matlab_raster_dir_name) - 4)
      }

      # append Rda
      temp_r_raster_dir_name <- paste0(temp_r_raster_dir_name, "_rda/")

      list(
        helpText(paste0("We can bin raster data in .mat format, but do you want to create raster data in .Rda format? ",
                        "Benefits include the option to plot raster data ")),
        textInput("bin_new_raster",
                  "New raster directory name (e.g., data/raster/Zhang_Desimone_7objects_raster_data_rda; by default, we append '_rda' to the matlab raster directory name)",
                  temp_r_raster_dir_name),
        numericInput("bin_raster_start_ind",
                     "Index of the sample where the new raster data begin",
                     value = NULL),
        numericInput("bin_raster_end_ind",
                     "Index of the sample where the new raster data end",
                     value = NULL),

        actionButton("bin_create_raster", "Create raster"))
    }
  })

  output$bin_show_create_bin_function_run = renderText({
    rv$create_bin_function_run
  })

  output$bin_show_create_raster_function_run = renderText(({
    rv$create_raster_funciton_run
  }))

  output$bin_show_chosen_raster = renderText({
    req(rv$raster_cur_dir_name)
    if(is.na(rv$raster_cur_dir_name)){
      "No file chosen yet"
    } else{
      basename(rv$raster_cur_dir_name)

    }
  })

  output$bin_show_raster_cur_file_name = renderText({
    paste0("current data shown:", "\n", rv$raster_cur_file_name)

  })

  output$bin_raster_plot = renderPlot({
    req(rv$mRaster_cur_data)
    temp_dfMelted <- reshape2::melt(rv$mRaster_cur_data)
    #trials/rownames are converted from character to integer by melt. Times/colnames are also integer
    if(length(unique(factor(temp_dfMelted$value))) < 3){
      ggplot(temp_dfMelted, aes(x = Var2, y = Var1)) +
        geom_raster(aes(fill=factor(value))) +
        scale_fill_manual(values=c("0"="white", "1"="black"))+
        labs(x="Time (ms)", y="Trial")+
        theme(legend.position="none")
    } else {
      ggplot(temp_dfMelted, aes(x = Var2, y = Var1)) +
        geom_raster(aes(fill=value)) +
        scale_fill_gradient(low="grey90", high="red")+
        labs(x="Time (ms)", y="Trial")+
        theme(legend.position="none")
    }

  })

  output$bin_PSTH = renderPlot({
    req(rv$mRaster_cur_data)

    temp_mRaster_cur_data_mean <- colSums(rv$mRaster_cur_data, na.rm = FALSE, dims = 1)/nrow(rv$mRaster_cur_data)
    temp_dfRaster_mean <- data.frame(time = as.numeric(names(temp_mRaster_cur_data_mean)), spike_mean_over_trials = temp_mRaster_cur_data_mean)

    qplot(x = time, y = spike_mean_over_trials, data = temp_dfRaster_mean, geom = "point", color = "salmon1") +
      scale_x_continuous(breaks = temp_dfRaster_mean$time[c(TRUE, rep(FALSE, length(temp_dfRaster_mean$time)/10))]) +
      labs(x="Time (ms)", y="Spike Mean over Trials") +
      theme(legend.position="none")
  })



  reactive_all_levels_of_basic_labels <- reactive({
    req(rv$binned_file_name)
    binned_data = rv$binned_data
    levels(factor(binned_data[[paste0("labels.",input$DS_basic___p___labels)]]))
  })


  reactive_all_levels_of_gen_var_to_use <- reactive({
    req(rv$binned_file_name)
    binned_data = rv$binned_data
    levels(factor(binned_data[[paste0("labels.",input$DS_gen___p___labels)]]))
  })



  output$DC_offer_scriptize = renderUI({
    list(
      #textInput("DC_to_be_saved_result_name",
      #          "File name of the result to be saved (e.g., my_results)"),
      #actionButton("DC_scriptize", "Generate script"),
      actionButton("DC_run_script", "Run script"),
      uiOutput("DC_scriptize_error")
    )
  })

  output$DC_offer_run_decoding = renderUI({
    list(
      helpText(""),
      actionButton("DC_run_decoding", "Save the script and run decoding"),
      uiOutput("DC_run_decoding_error")
    )
  })

  output$DS_offer_upload_bin = renderUI({
    list(
      fileInput("DS_uploaded_binned",
                "Upload new binned data (optional)",
                multiple = TRUE),
      textInput("DS_uploaded_binned_name",
                "Where you want this file to be saved",
                rv$binned_base_dir),
      actionButton("DS_save_binned_to_disk","Save to disk"),
      uiOutput("DS_save_binned_to_disk_error")

    )
  })


  output$where = renderDataTable(input$bin_uploaded_raster)



  output$bin_offer_upload_raster = renderUI({
    list(
      fileInput("bin_uploaded_raster",
                "Upload a zipped file raster data",
                multiple = TRUE),
      textInput("bin_uploaded_raster_name",
                "Where you want the file to be unzipped",
                rv$raster_base_dir),
      actionButton("bin_save_raster_to_disk", "Save to disk"),
      uiOutput("bin_save_raster_to_disk_error")
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
    binned_data = rv$binned_data
    tempLevels <- levels(unique(factor(binned_data$siteID)))
    if (is.null(input$DS_basic___p___site_IDs_to_use)){
      tempLevels
    }else{
      tempLevels[-(which(tempLevels %in% input$DS_basic___p___site_IDs_to_use))]
    }

  })


  reactive_all_gen_site_IDs_to_use <- reactive({
    req(rv$binned_file_name)
    binned_data = rv$binned_data
    levels(unique(factor(binned_data$siteID)))
  })

  reactive_all_gen_site_IDs_to_exclude <- reactive({
    req(rv$binned_file_name)
    binned_data = rv$binned_data
    tempLevels <- levels(unique(factor(binned_data$siteID)))

    if (is.null(input$DS_gen___p___site_IDs_to_use)){
      tempLevels
    }else{
      tempLevels[-(which(tempLevels %in% input$DS_gen___p___site_IDs_to_use))]
    }
  })



  reactive_DS_levels_to_use <- reactive({
    req(rv$binned_data)
    if(input$DS_type == "ds_basic"){
      validate(
        need(!is.null(input$DS_basic___p___label_levels)||!input$DS_basic___np___select_levels, paste0("You haven't set your levels yet")))

      if(!input$DS_basic___np___select_levels){
        reactive_all_levels_of_basic_labels()
      }else{
        input$DS_basic___p___label_levels
      }
    } else{

      all_labels <- c()
      for (class_i in 1:input$DS_gen___np___class_number) {
        curr_train_labels <- eval(str2lang(paste0("input$DS_gen___p___train_label_levels_class_", class_i)))
        curr_test_labels <- eval(str2lang(paste0("input$DS_gen___p___test_label_levels_class_", class_i)))
        all_labels <- c(all_labels, curr_train_labels, curr_test_labels)
      }


      all_labels



    }  # end else statement for the ds_generalization


  })

  reactive_level_repetition_info_each_site <- reactive({
    req(reactive_DS_levels_to_use())
    if(input$DS_type == "ds_basic"){
      num_label_reps <- NeuroDecodeR:::get_num_label_repetitions_each_site(rv$binned_data,
                                                                input$DS_basic___p___labels,
                                                                label_levels = reactive_DS_levels_to_use())
    }else{
      num_label_reps <- NeuroDecodeR:::get_num_label_repetitions_each_site(rv$binned_data,
                                                                input$DS_gen_var_to_use,
                                                                label_levels = reactive_DS_levels_to_use())
    }
    num_label_reps
  })



#### Basic Outputs ----

  output$DS_basic___p___list_of_labels = renderUI({
    req(rv$binned_file_name)
    selectInput("DS_basic___p___labels",
                "Variable to decode and to use",
                rv$binned_all_var)
  })

  output$DS_basic___np___list_of_levels_to_use = renderUI({
    selectInput("DS_basic___p___label_levels",
                "Levels to use",
                reactive_all_levels_of_basic_labels(),
                multiple = TRUE)
  })

  output$DS_basic___p___use_count_data = renderUI({
    selectInput("DS_basic___p___use_count_data",
                "Convert the data into spike counts",
                c(FALSE, TRUE))
  })


  output$DS_basic___p___site_IDs_to_use = renderUI({
    selectInput("DS_basic___p___site_IDs_to_use",
                "Which sites should be used",
                reactive_all_basic_site_IDs_to_use(),
                multiple = TRUE)
  })

  output$DS_basic___p___site_IDs_to_exclude = renderUI({
    selectInput("DS_basic___p___site_IDs_to_exclude",
                "Which sites should be excluded",
                reactive_all_basic_site_IDs_to_exclude(),
                multiple = TRUE)
  })

  output$DS_basic___p___randomly_shuffled_labels = renderUI({
    selectInput("DS_basic___p___randomly_shuffled_labels",
                "Randomly shuffle labels",
                c(FALSE, TRUE))
  })

  output$DS_basic___p___create_simultaneous_populations = renderUI({
    selectInput("DS_basic___p___create_simultaneous_populations",
                 "Was the data created simultaneously?",
                 c(0,1))
  })


#### General Output ----

  output$DS_gen___np___list_of_labels = renderUI({
    req(rv$binned_file_name)
    selectInput("DS_gen___p___labels",
                "Variable to decode and to use",
                rv$binned_all_var)
  })

  rv$prev_bins <- NULL
  rv$gen_bins <- NULL

  # Append new value to previous values when input$DS_gen___np___class_number changes
  observeEvent(input$DS_gen___np___class_number, {
    rv$prev_bins <- c(tail(rv$prev_bins, 1), input$DS_gen___np___class_number)
  })


  output$DS_gen___p___label_levels = renderUI({

    req(rv$binned_file_name)
    req(input$DS_gen___np___class_number)

    train_lst <- list()
    test_lst <- list()
    for (i in 1:input$DS_gen___np___class_number){

      train_lst[[i]] <- selectInput(paste0("DS_gen___p___train_label_levels_class_", i),
                                    paste("Class",i, "- Training levels to use"),
                                    reactive_all_levels_of_gen_var_to_use(),
                                    multiple = TRUE)
      test_lst[[i]] <- selectInput(paste0("DS_gen___p___test_label_levels_class_", i),
                                   paste("Class",i, "- Testing levels to use"),
                                   reactive_all_levels_of_gen_var_to_use(),
                                   multiple = TRUE)
    }

    c(rbind(train_lst, test_lst))
  })

  #output$DS_gen___p___test_label_levels = renderUI({
  #  req(rv$binned_file_name)
  #  selectInput("DS_gen___p___test_label_levels",
  #              "Testing labels to use",
  #              reactive_all_levels_of_gen_test_var_to_use(),
  #              multiple = TRUE)
  #})

  output$DS_gen___p___use_count_data = renderUI({
    selectInput("DS_gen___p___use_count_data",
                "Convert the data into spike counts",
                c(FALSE, TRUE))
  })


  output$DS_gen___p___site_IDs_to_use = renderUI({
    selectInput("DS_gen___p___site_IDs_to_use",
                "Which sites should be used",
                reactive_all_gen_site_IDs_to_use(),
                multiple = TRUE)
  })

  output$DS_gen___p___site_IDs_to_exclude = renderUI({
    selectInput("DS_gen___p___site_IDs_to_exclude",
                "Which sites should be excluded",
                reactive_all_gen_site_IDs_to_exclude(),
                multiple = TRUE)
  })


  output$DS_gen___p___randomly_shuffled_labels = renderUI({
    selectInput("DS_gen___p___randomly_shuffled_labels",
                "Randomly shuffle labels",
                c(FALSE, TRUE))
  })

  output$DS_gen___p___create_simultaneous_populations = renderUI({
    selectInput("DS_basic___p___create_simultaneous_populations",
                "Was the data created simultaneously?",
                c(0,1))
  })


  #output$DS_gen_list_of_var_to_use = renderUI({
  #  req(rv$binned_file_name)
  #  selectInput("DS_gen_var_to_use",
  #              lLabels$DS_gen_var_to_use,
  #              rv$binned_all_var)
  #})

  #output$DS_gen___np___select_num_of_groups = renderUI({
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
  #TODO: max_repetition_avail_with_any_site?? maybe change to DS___np___max_repetition_avail_with_any_site
  observe({
    req(reactive_level_repetition_info())
    temp_level_rep_info <- reactive_level_repetition_info()
    if(input$DS_type == "ds_basic"){
      updateNumericInput(session, "DS_basic___p___num_label_repeats_per_cv_split",
                         min = floor(temp_level_rep_info$min_repeats/input$DS_basic___p___num_cv_splits))
      updateNumericInput(session, "DS_basic___p___num_cv_splits",
                         min = floor(temp_level_rep_info$min_repeats/input$DS_basic___p___num_label_repeats_per_cv_split))
    }else{
      updateNumericInput(session, "DS_gen___p___num_label_repeats_per_cv_splits",
                         min = floor(temp_level_rep_info$min_repeats/input$DS_gen___p___num_cv_splits))
      updateNumericInput(session, "DS_gen___p___num_cv_splits",
                         min = floor(temp_level_rep_info$min_repeats/input$DS_gen___p___num_label_repeats_per_cv_splits))
    }
  })

  reactive_level_repetition_info <- reactive({
    req(reactive_DS_levels_to_use())
    if(input$DS_type == "ds_basic"){
      num_label_reps <- NeuroDecodeR:::get_num_label_repetitions(rv$binned_data,
                                                                input$DS_basic___p___labels,
                                                                label_levels = reactive_DS_levels_to_use())
    }else{
      #TO DO what is this for gen?
      num_label_reps <- NeuroDecodeR:::get_num_label_repetitions(rv$binned_data,
                                                                input$DS_gen___p___labels,
                                                                label_levels = reactive_DS_levels_to_use())
    }
    num_label_reps
  })


  reactive_chosen_repetition_info <- reactive({
    if(input$DS_type == "ds_basic"){
      req(input$DS_basic___p___num_cv_splits, input$DS_basic___p___num_label_repeats_per_cv_split, reactive_level_repetition_info())
      temp_level_repetition_info <- reactive_level_repetition_info()
      list(num_repetition = input$DS_basic___p___num_label_repeats_per_cv_split * input$DS_basic___p___num_cv_splits,
      num_sites_avail = nrow(filter(temp_level_repetition_info, min_repeats >= input$DS_basic___p___num_label_repeats_per_cv_split * input$DS_basic___p___num_cv_splits)))

    }else{
      req(input$DS_gen___p___num_cv_splits, input$DS_gen___p___num_label_repeats_per_cv_split, reactive_level_repetition_info())
      temp_level_repetition_info <- reactive_level_repetition_info()
      list(num_repetition = input$DS_gen___p___num_label_repeats_per_cv_split * input$DS_gen___p___num_cv_splits,
           num_sites_avail = nrow(filter(temp_level_repetition_info, min_repeats >= input$DS_gen___p___num_label_repeats_per_cv_split * input$DS_gen___p___num_cv_splits)))
    }
  })




  #### General Outputs ----

  output$DS___np___max_repetition_avail_with_any_site <- renderText({
    req(reactive_level_repetition_info_each_site())
    temp_level_rep_info <- reactive_level_repetition_info_each_site()
    paste("Levels chosen for training:", "<font color='red'>",
          paste(reactive_DS_levels_to_use(), collapse = ', '),
          "<br/>", "</font>", "The maximum number of repetitions across all the levels for training as set on the Data Source tab is",
          "<font color='red'>",
          min(temp_level_rep_info$min_repeats), "</font>", ".")
  })

  output$DS_show_chosen_repetition_info <- renderText({
    req(reactive_level_repetition_info_each_site())
    temp_chosen_repetition_info <- reactive_level_repetition_info_each_site()
    num_repetitions <- input$DS_basic___p___num_label_repeats_per_cv_split * input$DS_basic___p___num_cv_splits
    num_usable_sites <- sum(temp_chosen_repetition_info$min_repeats >= num_repetitions)
    if (input$DS_type == "ds_basic"){
      paste("You selected", "<font color='red'>",
            num_repetitions, "</font>",
            "trials (", input$DS_basic___p___num_label_repeats_per_cv_split,
            " repeats x ",  input$DS_basic___p___num_cv_splits,
            "CV splits). Based on the levels selected Data source tab, this gives <font color='red'>"
            , num_usable_sites, "</font>", " sites available for decoding.")
    }else{
      paste("You selected", "<font color='red'>",
            temp_chosen_repetition_info$num_repetition, "</font>",
            "trials (", input$DS_gen___p___num_label_repeats_per_cv_split,
            " repeats x ",  input$DS_gen___p___num_cv_splits,
            "CV splits). Based on the levels selected Data source tab, this gives <font color='red'>")
      #,temp_chosen_repetition_info$num_sites_avail, "</font>", " sites available for decoding.")
    }

  })

  #### Basic Outputs ----

  output$DS_basic___p___num_label_repeats_per_cv_split = renderUI({
    numericInput("DS_basic___p___num_label_repeats_per_cv_split",
                 "Number of repeats of each level in each CV split",
                 value = 1, min = 2)
  })

  output$DS_basic___p___num_cv_splits = renderUI({
    req(rv$binned_file_name)
    numericInput("DS_basic___p___num_cv_splits",
                 "Number of cross validation splits",
                 value = 2, min = 2)

  })

  output$DS_basic___p___num_resample_sites = renderUI({
    numericInput("DS_basic___p___num_resample_sites",
                 "Number of resampling sites",
                 value = NULL, min = 1)

  })


  #### General Outputs ----

  output$DS_gen___p___num_label_repeats_per_cv_split = renderUI({
    numericInput("DS_gen___p___num_label_repeats_per_cv_split",
                 "Number of repeats of each level in each CV split",
                 value = 1, min = 1)
  })

  output$DS_gen___p___num_cv_splits = renderUI({
    req(rv$binned_file_name)
    numericInput("DS_gen___p___num_cv_splits",
                 "Number of cross validation splits",
                 value = 2, min = 2)

  })

  output$DS_gen___p___num_resample_sites = renderUI({
    numericInput("DS_gen___p___num_resample_sites",
                 "Number of resampling sites",
                 value = NULL, min = 1)

  })

  #### Plot ----
  output$DS_show_level_repetition_info <- renderPlotly({

    # unfortunately code somewhere around here leads to a warning that says:
    #   Warning: Error in select: Can't subset columns that don't exist.
    #   ✖ Column `labels.` doesn't exist.
    #   140:
    # I can't trace where this warning is coming from so I am going to ignore it for now

    req(reactive_level_repetition_info())
    temp_level_repetition_info <- reactive_level_repetition_info()
    ggplotly(plot(temp_level_repetition_info))
  })


  ### Feature Processing ----
  #### Reactive Functions ----
  reactive_all_fp_avail <- reactive({
    req(input$CL_type)
    all_fp[cl_fp[,input$CL_type]>0]
  })

  reactive_bin_num_neuron <- reactive({
    validate(
      need(input$DS___p___binned_data,"Please select data source first to get total number of neurons")
    )
    binned_data = rv$binned_data
    length(unique(factor(binned_data$siteID)))
  })


  #### Output ----
  output$FP_type = renderUI({
    checkboxGroupInput("FP_type",
                       "Feature Preprocessors",
                       reactive_all_fp_avail(),
                       selected = "fp_zscore")
  })

  output$FP_skf___p___num_sites_to_use = renderUI({
    req(input$FP_type)
    if("fp_select_k_features" %in% input$FP_type){
      numericInput("FP_skf___p___num_sites_to_use",
                   "Select top features? (this will be applied first)",
                   reactive_bin_num_neuron(),
                   min = 1,
                   max = reactive_bin_num_neuron())
    }
  })

  output$FP_skf___p___num_sites_to_exclude = renderUI({
    req(input$FP_skf___p___num_sites_to_use)
    if("fp_select_k_features" %in% input$FP_type){
      numericInput("FP_skf___p___num_sites_to_exclude",
                   "exclude top ? features (this will be applied second)",
                   value = 0,
                   min = 0,
                   max = reactive_bin_num_neuron() - input$FP_skf___p___num_sites_to_use)
    }
  })


  ### Result Metrics ----
  output$RM_type = renderUI({
    checkboxGroupInput("RM_type",
                       "Result Metrics",
                       all_rm,
                       selected = all_rm)
  })


  #### Output ----
  output$RM_mr___np___include_norm_rank_results_text = renderText({
    if("rm_main_results" %in% input$RM_type){
      "<br>Parameters for rm_main_results:"
    }
  })
  output$RM_mr___p___include_norm_rank_results = renderUI({
    #req(input$RM_mr___p___include_norm_rank_results)
    if("rm_main_results" %in% input$RM_type){
      checkboxInput("RM_mr___p___include_norm_rank_results",
                   " Include normalized rank results", value = TRUE)
    }
  })


  output$RM_cm___np___text = renderText({
    if("rm_confusion_matrix" %in% input$RM_type){
      "<br>Parameters for rm_confusion_matrix:"
    }
  })
  output$RM_cm___p___save_TCD_results = renderUI({
    #req(input$RM_mr___p___include_norm_rank_results)
    if("rm_confusion_matrix" %in% input$RM_type){
      checkboxInput("RM_cm___p___save_TCD_results",
                    " Save results only for training and testing at the same time",
                    value = TRUE, width = '100%')
    }
  })
  output$RM_cm___p___create_decision_vals_confusion_matrix = renderUI({
    #req(input$RM_mr___p___include_norm_rank_results)
    if("rm_confusion_matrix" %in% input$RM_type){
      checkboxInput("RM_cm___p___create_decision_vals_confusion_matrix",
                    " Create decision value confusion matrix", value = TRUE)
    }
  })






  ### Cross-Validator ----
  #### Output ----
  output$CV___p___num_resample_runs = renderUI({
    numericInput("CV___p___num_resample_runs",
                 "Number of resampling runs",
                 value = 50, min = 1)

  })

  output$CV___p___num_parallel_cores = renderUI({
    numericInput("CV___p___num_parallel_cores",
                 "Number of parallel cores for resample runs",
                 value = NULL)
  })

  output$CV___p___parallel_outfile = renderUI({
    req(!is.null(input$CV___p___num_parallel_cores))
    req(input$CV___p___num_parallel_cores >= 1)

    textInput("CV___p___parallel_outfile",
                 "File name of output from parallel cores",
                 value = NULL)
  })



  ### Run Decoding ----

  output$DC_ace = renderUI({

    if (input$DC_script_mode == "Matlab") {
      script_editor_mode <- "matlab"
    } else {
      script_editor_mode <- "r"
    }


    ace_editor <- shinyAce::aceEditor("script",
                        rv$displayed_script,
                        mode = script_editor_mode)


    update_ace_editor_code()


    ace_editor


  })




  # a helper function that refreshes what is on the ace editor
  update_ace_editor_code <- function() {

    if (rv$displayed_script == "") {

      curr_radio_button_setting <- input$DC_script_mode

      # to make sure the editor shows the script the first time one clicks on a tab
      #  one needs to chance the radio button to a different choice and then back again
      #  (definitely a bit of a hack but it seems to work fairly well)

      updateRadioButtons(session, "DC_script_mode", "File type for generated script",
                         c("R", "R Markdown", "Matlab"), selected = "R Markdown")

      updateRadioButtons(session, "DC_script_mode", "File type for generated script",
                         c("R", "R Markdown", "Matlab"), selected = "R")

      updateRadioButtons(session, "DC_script_mode", "File type for generated script",
                         c("R", "R Markdown", "Matlab"), selected = curr_radio_button_setting)

    }

  }




  # Trying to get the script to update when the include comments box is
  #  checked/unchecked but this is not working :(

  observeEvent(input$include_comments,{


    # unfortunately call this function does nothing so need to manually write the code to do this
    # update_ace_editor_code()

    curr_radio_button_setting <- input$DC_script_mode

    updateRadioButtons(session, "DC_script_mode", "File type for generated script",
                     c("R", "R Markdown", "Matlab"), selected = "R Markdown")

    updateRadioButtons(session, "DC_script_mode", "File type for generated script",
                     c("R", "R Markdown", "Matlab"), selected = "R")

    updateRadioButtons(session, "DC_script_mode", "File type for generated script",
                     c("R", "R Markdown", "Matlab"), selected = curr_radio_button_setting)

  })





  observeEvent(input$DC_script_mode,{

    # Can't generate a script if a particular binned data file has not been selected
    req(input$DS___p___binned_data)

    rv_para$id <-  c("DS___p___binned_data", "DS_type", "DC_to_be_saved_result_name")


    # Data Source

    # For ds_basic parameters
    if(input$DS_type == "ds_basic"){
      rv_para$id <- c(rv_para$id,"DS_basic___p___labels",
                      "DS_basic___p___num_label_repeats_per_cv_split", "DS_basic___p___num_cv_splits",
                      "DS_show_chosen_repetition_info", "DS_basic___p___num_resample_sites")
      if(input$DS_basic___np___select_levels){
        rv_para$id <- c(rv_para$id,  "DS_basic___p___label_levels")
      }
      if(input$DS_basic___np___advanced){
        rv_para$id <- c(rv_para$id,  "DS_basic___p___use_count_data", "DS_basic___p___site_IDs_to_use",
                        "DS_basic___p___site_IDs_to_exclude", "DS_basic___p___randomly_shuffled_labels",
                        "DS_basic___p___create_simultaneous_populations")
      }

    #ds_gen
    } else {
      rv_para$id <- c(rv_para$id, "DS_gen___np___class_number", "DS_basic___p___labels",
                      "DS_gen___p___num_cv_splits","DS_gen___p___num_label_repeats_per_cv_split",
                      "DS_gen___p___num_resample_sites")


      for (class_i in 1:input$DS_gen___np___class_number){
        rv_para$id <- c(rv_para$id,
                        paste0("DS_gen___p___train_label_levels_class_",  class_i),
                        paste0("DS_gen___p___test_label_levels_class_",  class_i))
      }

      if(input$DS_gen___np___advanced){
        rv_para$id <- c(rv_para$id, "DS_gen___p___use_count_data","DS_gen___p___site_IDs_to_use",
                        "DS_gen___p___site_IDs_to_exclude", "DS_gen___p___randomly_shuffled_labels",
                        "DS_gen___p___create_simultaneous_populations")
      }
    }


    # Classifier
    rv_para$id <- c(rv_para$id, "CL_type")
    if(input$CL_type == 'cl_svm'){
      rv_para$id <- c(rv_para$id, "CL_svm___p___kernel", "CL_svm___p___cost")
      if(input$CL_svm___p___kernel == 'polynomial'){
        rv_para$id <- c(rv_para$id, "CL_svm___p___degree", "CL_svm___p___coef0", "CL_svm___p___gamma")
      }
      if(input$CL_svm___p___kernel == 'radial'){
        rv_para$id <- c(rv_para$id, "CL_svm___p___coef0", "CL_svm___p___gamma")
      }
      if(input$CL_svm___p___kernel == 'sigmoid'){
        rv_para$id <- c(rv_para$id, "CL_svm___p___gamma")
      }
    }


    #Feature Preprocessors
    rv_para$id <- c(rv_para$id, "FP_type")
    if ('fp_select_k_features' %in% input$FP_type) {
      rv_para$id <- c(rv_para$id, "FP_skf___p___num_sites_to_use",
                      "FP_skf___p___num_sites_to_exclude")
    }


    #Result Metrics
    rv_para$id <- c(rv_para$id, "RM_type")
    if ('rm_main_results' %in% input$RM_type) {
      rv_para$id <- c(rv_para$id, "RM_mr___p___include_norm_rank_results")
    }
    if ('rm_confusion_matrix' %in% input$RM_type){
      rv_para$id <- c(rv_para$id, "RM_cm___p___save_TCD_results",
                      "RM_cm___p___create_decision_vals_confusion_matrix")
    }

    #Cross Validator
    rv_para$id <- c(rv_para$id, "CV___p___run_TCD",
                    "CV___p___num_resample_runs")

    if(!is.null(input$CV___p___num_parallel_cores) &&
       !is.na(input$CV___p___num_parallel_cores) &&
       input$CV___p___num_parallel_cores >= 1) {
        rv_para$id <- c(rv_para$id, "CV___p___num_parallel_cores",
                        "CV___p___parallel_outfile")
    }

    rv_para$id <- c(rv_para$id, "include_comments")

    rv_para$inputIDs <- paste0("input$", rv_para$id)
    rv_para$values <- lapply(rv_para$inputIDs, function(i){
      eval(str2lang(i))
    })

    decoding_params <- rv_para$values
    decoding_params <- setNames(decoding_params, rv_para$id)


    # add the directory name of the binned data to the decoding params
    decoding_params$working_dir <- working_dir
    decoding_params$binned_dir_name <- rv$binned_base_dir
    decoding_params$results_dir_name <- rv$result_base_dir #might break this


    #decoding_params$include_comments <- FALSE

    if (input$DC_script_mode == "R") {
      rv$displayed_script <- generate_r_script_from_shiny_decoding_params(decoding_params)
    } else if (input$DC_script_mode == "R Markdown") {
      rv$displayed_script <- generate_r_markdown_from_shiny_decoding_params(decoding_params)
    }


    rv$displayed_script


  })




  # Create and run R code

  observeEvent(input$DC_run_script,{


    if (input$DC_script_mode == "R") {
      script_save_dir <- file.path("", "r_scripts", "")
      file_extension <- ".R"
    } else if (input$DC_script_mode == "R Markdown") {
      script_save_dir <- file.path("", "r_markdown", "")
      file_extension <- ".Rmd"
    }


    # generate analysis script name

    # should perhaps do this when the script is generated and then can add the script name as meta
    #  data to be saved with the decoding results, but ok for now...
    # ELISA
    script_file_name <- paste0(result_base_dir, script_save_dir,
                               "NeuroShiny_Script_ID_",
                               NeuroDecodeR:::generate_analysis_ID(),
                               file_extension)


    # write the code to a script
    fileConn <- file(script_file_name)
    writeLines(rv$displayed_script, fileConn)
    close(fileConn)



    # run the script
    if (input$DC_script_mode == "R") {
      source(script_file_name)
    } else if (input$DC_script_mode == "R Markdown") {


      rv$pdf_knitting_status <- "running"

      pdf_file_name <- stringr::str_replace(script_file_name, "r_markdown", "r_markdown_pdf")
      pdf_file_name <- stringr::str_replace(pdf_file_name, "Rmd$", "pdf")

      rv$latest_pdf_file_name <- basename(pdf_file_name)

      # add a notification that the R Markdown document is knitting
      running_id <- showNotification("Knitting R Markdown results...",
                                     duration = NULL,
                                     closeButton = FALSE,
                                     type = "message")
      on.exit(removeNotification(running_id), add = TRUE)


      rmarkdown::render(script_file_name, "pdf_document", output_dir = dirname(pdf_file_name))


      rv$pdf_knitting_status <- "completed"
      file.copy(pdf_file_name, paste0("www/", basename(pdf_file_name)))


    }  # end for creating an R Markdown document



  })


  output$DC_plot_pdf <- renderUI({

    if (is.null(rv$pdf_knitting_status)) {
      return("No R Markdown results have been knit yet")
    } else if (rv$pdf_knitting_status == "running") {
      # This is never run b/c when code is knitting it holds up any updates to the UI :(
      # I think I will need to use the promise package to knit asynchronously to get this to work :/
      return("R Markdown results are in the process of being knit a pdf...")
    } else {
      tags$iframe(style="height:600px; width:100%", src = rv$latest_pdf_file_name)
    }


  })




  output$DC_show_chosen_script = renderText({
    basename(rv$script_chosen)
  })

  output$DC_ace = renderUI({
    shinyAce::aceEditor("script",
                        rv$displayed_script,
                        mode = input$DC_script_mode)
  })

  output$DC_pdf <- renderUI({
    if (is.null(rv$save_script_name)){
      "The results will appear as a pdf below once the code is done running."
    }else{
      pdf_name <- gsub("Rmd", "pdf", basename(rv$save_script_name))
      tags$iframe(style="height:600px; width:100%", src = pdf_name)
    }
  })

  observeEvent(input$Plot_create_pdf,{
    req(rv$result_chosen, input$Plot_timeseries_result_type)
    append_result_to_pdf_and_knit(rv$result_chosen, input$Plot_timeseries_result_type)
    print("done")
    output$Plot_pdf <- renderUI({
      req(rv$result_chosen)
      pdf_name <- gsub("Rmd", "pdf", rv$save_script_name)
      tags$iframe(style="height:600px; width:100%", src = pdf_name)
    })

  })

  observeEvent(input$DC_save_displayed_script,{
    req(input$DC_to_be_saved_script_name, rv$displayed_script)
    temp_file_name = file.path(script_base_dir, input$DC_to_be_saved_script_name)
    file.create(temp_file_name, overwrite = TRUE)
    write(rv$displayed_script, file = temp_file_name)
  })

  observeEvent(input$DC_run_decoding, {
    req(input$DC_to_be_saved_result_name, rv$displayed_script)

    # add the appropriate file extenstion to the saved file name
    if(input$DC_script_mode == "R Markdown"){
      file_extension <- ".Rmd"
    } else {
      file_extension <- ".R"
    }

    file_pieces <- unlist(base::strsplit(input$DC_to_be_saved_result_name, "[.]"))

    if(length(file_pieces) == 1){
      save_file_name <- paste0(file_pieces[1], file_extension)
    } else{
      if(!(file_pieces[length(file_pieces)] == file_extension)){
        save_file_name <- paste0(save_file_name, file_extension)
      }else{
        save_file_name <- input$DC_to_be_saved_result_name
      }
    }

    save_script_name <- file.path(script_base_dir, save_file_name)
    write(rv$displayed_script, file = save_script_name)
    rv$save_script_name <- save_script_name

    if(input$DC_script_mode == "R Markdown") {
      rmarkdown::render(save_script_name, "pdf_document", output_dir = "www")
    }else{
      source(save_script_name)
    }
  })

  ## Plotting Results ----

  observe({
    req(input$Plot_chosen_result)
    temp_df_file <- shinyFiles::parseFilePaths(c(wd= rv$result_base_dir),input$Plot_chosen_result)
    req(temp_df_file$datapath)
    rv$result_chosen <- temp_df_file$datapath
    load(rv$result_chosen)
    rv$result_data <- DECODING_RESULTS
  })

  output$Plot_show_chosen_result = renderText({
    if(is.na(rv$result_chosen)){
      "No file chosen yet"
    } else{
      basename(rv$result_chosen)
    }
  })

  output$Plot_timeseries = renderPlot({
    req(rv$result_data)
    plot(rv$result_data$rm_main_results, plot_type = "line",
         result_type = input$Plot_timeseries_result_type)
  })

  output$Plot_tct = renderPlot({
    req(rv$result_data)
    plot(rv$result_data$rm_main_results,
         result_type = input$Plot_timeseries_result_type)

  })

# Error Messages ----

  er_scriptize_action_error <- eventReactive(rv_para$decoding_para_id_computed,{
    # if we don't have this line, this function will be called as soon as users click the script tab because rv_para$decoding_para_id_computed is going from NULL to 1 (I think)
    req(rv_para$id)
    validate(
      need(input$DS_chosen_bin, "Did you not even choose the binned data?")
    )
    temp_need = lapply(rv_para$id, function(i){
      eval(parse(text = paste0("need(input$", i, ", '", "You need to set your parameter first')")))
    })
    do.call(validate, temp_need)
  })

  output$DC_scriptize_error <- renderText({
    er_scriptize_action_error()
  })

  er_bin_action_error <- eventReactive(input$bin_bin_data,{
    validate(
      need(rv$raster_cur_dir_name, "You haven't chosen the raster data yet!")
    )
    validate(
      need(rv$raster_bRda||rv$raster_bMat, "We only accept .mat and .Rda format !")
    )
  })

  er_bin_save_raster_to_disk_error <- eventReactive(input$bin_save_raster_to_disk,{
    validate(
      need(input$bin_uploaded_raster, "Please upload a zipped file raster data"),
      need(input$bin_uploaded_raster_name, "Please tell me where you want the file to be unzipped" )
    )
  })

  er_DS_save_binned_to_disk_error <- eventReactive(input$DS_save_binned_to_disk,{
    validate(
      need(input$DS_uploaded_binned, "Please upload new binned data"),
      need(input$DS_uploaded_binned_name, "Please tell me where you want this file to be saved")
    )
  })

  er_DC_save_displayed_script_error <- eventReactive(input$DC_save_displayed_script,{
    validate(
      need(rv$displayed_script,"Please generate the script first"),
      need(input$DC_to_be_saved_script_name, "Please tell me the new script name")
    )
  })

  output$bin_action_error = renderUI({
    er_bin_action_error()

  })

  output$bin_save_raster_to_disk_error = renderUI({

    er_bin_save_raster_to_disk_error()

  })

  output$DS_save_binned_to_disk_error = renderUI({
    er_DS_save_binned_to_disk_error()
  })

  output$DC_save_displayed_script_error = renderUI({
    er_DC_save_displayed_script_error()
  })

  output$DC_run_decoding_error = renderUI({
    er_DC_save_displayed_script_error()
  })

  output$bin_evil_raster = renderUI({
    req(rv$raster_cur_dir_name)
    validate(
      need(reactive_bRaster_qualified() > 0, "Only accept .mat and .Rda format. Please change your dataset file type"))
  })

  er_scriptize_action_error <- eventReactive(rv_para$decoding_para_id_computed,{
    req(rv_para$id)
    validate(
      need(input$DS___p___binned_data, "No data has been uploaded")
    )
    temp_need = lapply(rv_para$id, function(i){
      eval(parse(text = paste0("need(input$", i, ", '", "Don't forget to set it first)")))
    })
    do.call(validate, temp_need)
  })



}
