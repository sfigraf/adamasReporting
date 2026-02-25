
getLengthFrequenciesGraph <- function(data, lengthOptions, binwidth, rsdLimits){
  
  if(lengthOptions == "RSD"){
    
    sampleFDataRSD <- data %>%
      #can't just count rsd and species bc there might be more than one fish in the numfish column
      #like the 5000 "Stock" sized carp in 2008 lol
      #shoudl do this all the time in this data, not use count()
      group_by(RSD, SpeciesCode) %>%
      summarise(Count = sum(NumFish)) %>%
      left_join(rsdLimits, by = "SpeciesCode") %>%
      mutate(RSD = replace_na(RSD, "Below Stock Size")) %>%
      mutate(hoverText = case_when(
        RSD == "Below Stock Size" ~ paste0(CommonName, " (", SpeciesCode, ")", "<br> Below Stock Size: <", SLEN, "mm <br>", "Count: ", Count), 
        RSD == "Stock" ~ paste0(CommonName, " (", SpeciesCode, ")", "<br> 'Stock' Range (mm): ", SLEN, " - ", QLEN, "<br>", "Count: ", Count), 
        RSD == "Quality" ~ paste0(CommonName, " (", SpeciesCode, ")", "<br> 'Quality' Range (mm): ", QLEN, " - ", PLEN, "<br>", "Count: ", Count), 
        RSD == "Preferred" ~ paste0(CommonName, " (", SpeciesCode, ")", "<br> 'Preferred' Range (mm): ", PLEN, " - ", MLEN, "<br>", "Count: ", Count), 
        RSD == "Memorable" ~ paste0(CommonName, " (", SpeciesCode, ")", "<br> 'Memorable' Range (mm): ", MLEN, " - ", TLEN, "<br>", "Count: ", Count), 
        RSD == "Trophy" ~ paste0(CommonName, " (", SpeciesCode, ")", "<br> 'Trophy' Range (mm): ", TLEN, "+", "<br>", "Count: ", Count), 
        #with assigning "Below Stock Size" to the NA values, this should never come up so if it does it's worth investigating why
        TRUE ~ "No RSD Assigned"
      ), 
      RSD = factor(RSD, levels = c("Below Stock Size", "Stock", "Quality", "Preferred", "Memorable", "Trophy"))) %>%
      ungroup()
    plot <- ggplot(sampleFDataRSD, aes(x = RSD, y = Count, fill = CommonName, text = hoverText)) +
      geom_col() +
      labs(caption = "RSD (mm)")
    
  } else{ 
    plot <- data %>%
      #weighting by numFIsh allows to see total number of fish, not just count the rows
      #numfish will get summed for a certain bin
      ggplot(aes(x = .data[[lengthOptions]], weight = NumFish, 
                 fill = CommonName)) +
      geom_histogram(binwidth = binwidth, 
                     aes(
                       group = CommonName,
                       label = CommonName,
                       text = paste0('Species: ', after_stat(label),
                                     "<br>Length ", if_else(lengthOptions == "Length_mm", "(mm)", "(inches)"), ' Range: ', after_stat(xmin), " to ", after_stat(xmax),  
                                     "<br>Count: ", after_stat(count)
                                     
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