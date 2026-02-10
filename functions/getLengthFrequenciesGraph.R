# data <- singlesurvey
# lengthOptions <- "Length_mm"
# binwidth = 10
getLengthFrequenciesGraph <- function(data, lengthOptions, binwidth, rsdLimits){
  if(lengthOptions == "RSD"){
    sampleFDataRSD <- data %>%
      #this works as long as there are no group RSD designations if there are a like 20 fish in the "numfish" column
      count(RSD, CommonName, name = "Count") %>%
      left_join(rsdLimits, by = "CommonName") %>%
      mutate(hoverText = case_when(RSD == "Stock" ~ paste0(CommonName, " 'Stock' Range (mm): ", SLEN, " - ", QLEN), 
                                   RSD == "Quality" ~ paste0(CommonName, " 'Quality' Range (mm): ", QLEN, " - ", PLEN), 
                                   RSD == "Preferred" ~ paste0(CommonName, " 'Preferred' Range (mm): ", PLEN, " - ", MLEN), 
                                   RSD == "Memorable" ~ paste0(CommonName, " 'Memorable' Range (mm): ", MLEN, " - ", TLEN), 
                                   RSD == "Trophy" ~ paste0(CommonName, " 'Trophy' Range (mm): ", TLEN, "+"), 
                                   TRUE ~ paste0(CommonName, ": No RSD Assigned")
                                   
      ), 
      RSD = factor(RSD, levels = c("Stock", "Quality", "Preferred", "Memorable", "Trophy"))) %>%
      ungroup()
    plot <- ggplot(sampleFDataRSD, aes(x = RSD, y = Count, fill = CommonName, text = hoverText)) +
      geom_col() +
      labs(caption = "RSD (mm)")
    
  } else{
    plot <- data %>%
      ggplot(aes(x = .data[[lengthOptions]], 
                 fill = CommonName)) +
      #theme_classic() +
      # labs(title = "Length Frequencies", caption = "Binwidth = 20mm") +
      geom_histogram(binwidth = binwidth, 
                     aes(
                       group = CommonName,
                       label = CommonName,
                       text = paste0("Length ", if_else(lengthOptions == "Length_mm", "(mm)", "(inches)"), ' Range: ', after_stat(xmin), " to ", after_stat(xmax),  
                                     "<br>Count: ", after_stat(count),
                                     '<br>Species: ', after_stat(label)
                       )
                     )) +
      labs(caption = paste(lengthOptions, ": Binwidth", binwidth))
  }
  plot <- plot + 
    theme_classic() +
    labs(title = "Length Frequencies") #, caption = "Binwidth = 20mm"
  
  #+
  #scale_fill_manual(values = allColors)
  plot <- ggplotly(plot, tooltip = "text")
  return(plot)
}