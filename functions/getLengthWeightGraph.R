#renders plotly length weight graph
getLengthWeightGraph <- function(data, lengthOptions, weightOptions){

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

  
  return(plot)
}