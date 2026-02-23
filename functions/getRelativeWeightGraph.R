getRelativeWeightGraph <- function(data, lengthOptions){
  
  #using .data pronoun to access column by string
  
  plot <- data %>%
    ggplot(aes(x = Wr, y = .data[[lengthOptions]], 
               color = CommonName, 
               text = paste0('Length: ', as.character(round(.data[[lengthOptions]], 2)),
                             '<br>Relative Weight: ', as.character(round(Wr, 2)), 
                             '<br>Species: ', CommonName, 
                             '<br>Survey ID: ', SurveyID
               )
    )) +
    geom_point() + 
    theme_classic() +
    labs(title = "Relative Weight Data") #+
  #scale_color_manual(values = allColors)
  plot <- ggplotly(plot, tooltip = "text")
  
  return(plot)
}