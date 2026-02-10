
getLengthFrequenciesGraph <- function(data, lengthOptions, binwidth, rsdLimits){
  if(lengthOptions == "RSD"){
    sampleFDataRSD <- data %>%
      #this works as long as there are no group RSD designations if there are a like 20 fish in the "numfish" column
      count(RSD, SpeciesCode, name = "Count") %>%
      left_join(rsdLimits, by = "SpeciesCode") %>%
      mutate(RSD = replace_na(RSD, "Below Stock Size")) %>%
      mutate(hoverText = case_when(
        RSD == "Below Stock Size" ~ paste0(CommonName, "<br> Below Stock Size: <", SLEN, "mm <br>", "Count: ", Count), 
        RSD == "Stock" ~ paste0(CommonName, "<br> 'Stock' Range (mm): ", SLEN, " - ", QLEN, "<br>", "Count: ", Count), 
        RSD == "Quality" ~ paste0(CommonName, "<br> 'Quality' Range (mm): ", QLEN, " - ", PLEN, "<br>", "Count: ", Count), 
        RSD == "Preferred" ~ paste0(CommonName, "<br> 'Preferred' Range (mm): ", PLEN, " - ", MLEN, "<br>", "Count: ", Count), 
        RSD == "Memorable" ~ paste0(CommonName, "<br> 'Memorable' Range (mm): ", MLEN, " - ", TLEN, "<br>", "Count: ", Count), 
        RSD == "Trophy" ~ paste0(CommonName, "<br> 'Trophy' Range (mm): ", TLEN, "+", "<br>", "Count: ", Count), 
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