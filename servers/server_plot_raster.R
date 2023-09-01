
################################################################################
############################### Raster file prep ###############################
################################################################################

# Update current neuron when clicking toggle buttons
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

# Show name of the current file
output$bin_show_raster_cur_file_name <- renderText({
  paste0("Current data shown (file ", rv$raster_cur_neuron, " of ",
         rv$raster_num_neuron, "):", "\n", rv$raster_cur_file_name)

})

# Setting up the current data to be viewed
observeEvent(list(rv$selected_rasters, rv$raster_cur_neuron),{
  if(rv$raster_num_neuron > 0){
    # Find the current file to view
    rv$raster_cur_file_name <- rv$raster_list[rv$raster_cur_neuron]

    # Load in the data and save time cols into matrix
    raster_data <- NeuroDecodeR::read_raster_data(file.path(rv$selected_rasters,
                                                            rv$raster_cur_file_name))

    raster_df <- select(raster_data, starts_with("time."))
    raster_matrix <- as.matrix(raster_df)

    # Reformat matrix and save as random variable
    rownames(raster_matrix) <- 1:dim(raster_matrix)[1]
    colnames(raster_matrix) <- gsub("time.", "", colnames(raster_matrix))
    colnames(raster_matrix) <- gsub("^(-?[0-9]+)_.*", "\\1", colnames(raster_matrix))
    rv$cur_raster_matrix <- raster_matrix
  } else if (rv$raster_num_neuron == 0){
    rv$raster_cur_file_name <- NULL
    rv$cur_raster_matrix <- NULL
  }
})

################################################################################
############################# Show raster plot #################################
################################################################################

# Render the raster plot of current file
output$bin_raster_plot <- renderPlot({
  req(rv$cur_raster_matrix)
  # Trials/rownames are converted from character to integer by melt
  # Times/colnames are also integer
  melted_raster <- reshape2::melt(rv$cur_raster_matrix)

  # Plot depending on how data is encoded (binary or larger scale)
  if(length(unique(factor(melted_raster$value))) <= 2){
    ggplot(melted_raster, aes(x = Var2, y = Var1)) +
      geom_raster(aes(fill = factor(value))) +
      scale_fill_manual(values = c("0" = "white", "1" = "black"))+
      labs(x = "Time (ms)", y = "Trial")+
      theme(legend.position="none")
  } else {
    ggplot(melted_raster, aes(x = Var2, y = Var1)) +
      geom_raster(aes(fill = value)) +
      scale_fill_gradient(low = "grey90", high = "red")+
      labs(x = "Time (ms)", y = "Trial")+
      theme(legend.position = "none")
  }
})

################################################################################
################################ Show PSTH plot ################################
################################################################################

output$bin_PSTH <- renderPlot({
  req(rv$cur_raster_matrix)
  # Create column means and turn into a df to plot
  cur_raster_col_means <- colMeans(rv$cur_raster_matrix, na.rm = FALSE)

  # Removing the ranges
  raster_means_df <- data.frame(time = as.numeric(names(cur_raster_col_means)),
                                   spike_mean_over_trials = cur_raster_col_means)

  # Create plot of avg spike over trials
  ggplot(data = raster_means_df, aes(x = time, y = spike_mean_over_trials)) +
    geom_point(color = "salmon1") +
    scale_x_continuous(breaks = raster_means_df$time[c(TRUE, rep(FALSE, length(raster_means_df$time) / 10))]) +
    labs(x = "Time (ms)", y = "Spike Mean Over Trials") +
    theme(legend.position = "none")
})




