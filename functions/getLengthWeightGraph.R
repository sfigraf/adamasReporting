getLengthWeightGraph <- function(data, lengthOptions, weightOptions){
  plot <- data %>%
    ggplot(aes(x = .data[[lengthOptions]], y = .data[[weightOptions]], 
               color = CommonName, 
               text = paste0('Length: ', as.character(round(.data[[lengthOptions]], 2)),
                             '<br>Weight: ', as.character(round(.data[[weightOptions]], 2)), 
                             '<br>Species: ', CommonName, 
                             '<br>Survey ID: ', SurveyID
               )
    )) +
    geom_point() + 
    theme_classic() +
    labs(title = "Length/Weight Data") #+
  #scale_color_manual(values = allColors)
  plot <- ggplotly(plot, tooltip = "text")
  
  return(plot)
}