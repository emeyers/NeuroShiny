

observe({
  req(input$bin_chosen_raster)

  rv$raster_cur_dir_name <- shinyFiles::parseDirPath(c(wd= rv$raster_base_dir),
                                                     input$bin_chosen_raster)
  req(rv$raster_cur_dir_name)
  mat_files_in_raster_dir <-
    list.files(rv$raster_cur_dir_name, pattern = "\\.mat$")

  if(length(mat_files_in_raster_dir) > 0){
    rv$raster_bMat <- TRUE
  } else {
    rv$raster_bMat <- FALSE
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
    } else{
      rv$raster_bRda <- FALSE
      # this doesn't work; observe is for action not calculation
      # validate("Only accept raster data in .mat or .Rda format !")

    }
  }
})


# bin_pre_neuron ----
observeEvent(input$bin_pre_neuron,{
  if(rv$raster_cur_neuron > 1){
    rv$raster_cur_neuron <- rv$raster_cur_neuron - 1
  }
})

# bin_next_neuron ----
observeEvent(input$bin_next_neuron,{
  if(rv$raster_cur_neuron < rv$raster_num_neuron){
    rv$raster_cur_neuron <- rv$raster_cur_neuron + 1
  }
})

# bin_show_raster_cur_file_name ----
output$bin_show_raster_cur_file_name = renderText({
  paste0("current data shown:", "\n", rv$raster_cur_file_name)

})


# bin_raster_plot ----
output$bin_raster_plot = renderPlot({
  req(rv$mRaster_cur_data)
  temp_dfMelted <- reshape2::melt(rv$mRaster_cur_data)
  # trials/rownames are converted from character to integer by melt.
  # Times/colnames are also integer
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


# bin_PSTH ----
output$bin_PSTH = renderPlot({
  req(rv$mRaster_cur_data)

  temp_mRaster_cur_data_mean <- colSums(rv$mRaster_cur_data,
                                        na.rm = FALSE,
                                        dims = 1)/nrow(rv$mRaster_cur_data)
  temp_dfRaster_mean <- data.frame(time = as.numeric(names(temp_mRaster_cur_data_mean)),
                                   spike_mean_over_trials = temp_mRaster_cur_data_mean)

  qplot(x = time, y = spike_mean_over_trials, data = temp_dfRaster_mean, geom = "point", color = "salmon1") +
    scale_x_continuous(breaks = temp_dfRaster_mean$time[c(TRUE, rep(FALSE, length(temp_dfRaster_mean$time)/10))]) +
    labs(x="Time (ms)", y="Spike Mean over Trials") +
    theme(legend.position="none")
})




