getRelativeWeightGraph <- function(data, lengthOptions){
  
  #using .data pronoun to access column by string
  
  plot <- data %>%
    plot_ly(
      x = ~Wr, 
      y = ~get(lengthOptions), 
      color = ~CommonName,
      type = 'scatter', 
      mode = 'markers',
      # Custom tooltip text
      text = ~paste0('Length: ', round(get(lengthOptions), 2),
                     '<br>Relative Weight: ', round(Wr, 2), 
                     '<br>Species: ', CommonName, 
                     '<br>Survey ID: ', SurveyID),
      hoverinfo = 'text'
    ) %>%
    layout(
      title = "Relative Weight Data",
      xaxis = list(title = "Relative Weight (Wr)"),
      yaxis = list(title = lengthOptions),
      # Mimics theme_classic()
      plot_bgcolor = 'rgba(0,0,0,0)', #'white',
      xaxis = list(showline = TRUE, linewidth = 1, linecolor = 'black', mirror = TRUE),
      yaxis = list(showline = TRUE, linewidth = 1, linecolor = 'black', mirror = TRUE)
    )
  return(plot)
}