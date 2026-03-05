#renders plotly length weight graph
getLengthWeightGraph <- function(data, lengthOptions, weightOptions){
  # message(paste("within length weight grpah colnames:", cat(colnames(data)))) # Look at the logs to see the real names
  # message(paste("within lw prah length options", lengthOptions))
  # message(nrow(data))
  # message(paste("trying to access the data:", data[[lengthOptions]]))
  #using .data pronoun to access column by string
  plot <- data %>%
    plot_ly(
      x = ~get(lengthOptions), 
      y = ~get(weightOptions), 
      color = ~CommonName,
      type = 'scatter', 
      mode = 'markers',
      # 'text' defines what shows up in the tooltip
      text = ~paste0('Length: ', round(get(lengthOptions), 2),
                     '<br>Weight: ', round(get(weightOptions), 2),
                     '<br>Species: ', CommonName,
                     '<br>Survey ID: ', SurveyID),
      hoverinfo = 'text'
    ) %>%
    layout(
      title = "Length/Weight Data",
      xaxis = list(title = lengthOptions),
      yaxis = list(title = weightOptions),
      # This mimics theme_classic()
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)'
    )
  # plot <- data %>%
  #   ggplot(aes(x = .data[[lengthOptions]], y = .data[[weightOptions]], 
  #              color = CommonName,
  #              text = paste0('Length: ', as.character(round(.data[[lengthOptions]], 2)),
  #                            '<br>Weight: ', as.character(round(.data[[weightOptions]], 2)),
  #                            '<br>Species: ', CommonName,
  #                            '<br>Survey ID: ', SurveyID
  #              )
  #   )) +
  #   geom_point() + 
  #   theme_classic() +
  #   labs(title = "Length/Weight Data") #+
  # #scale_color_manual(values = allColors)
  # plot <- ggplotly(plot, tooltip = "text")
  
  return(plot)
}