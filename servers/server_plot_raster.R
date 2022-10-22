
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




