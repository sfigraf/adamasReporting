
getLengthFrequenciesGraph <- function(data, lengthOptions, binwidth, rsdLimits){

  if(lengthOptions == "RSD"){
    
    sampleFDataRSD <- data %>%
      #can't just count rsd and species bc there might be more than one fish in the numfish column
      #like the 5000 "Stock" sized carp in 2008 lol
      #shoudl do this all the time in this data, not use count()
      group_by(RSD, SpeciesCode) %>%
      summarise(Count = sum(NumFish)) %>%
      left_join(rsdLimits, by = "SpeciesCode") %>%
      mutate(RSD = replace_na(RSD, "Below Stock Size"), 
             #soemtimes there aren't common names assigned in rsdLimits. change them to not be NA so plotly will "See" them in the graph and not make them tranparent
             CommonName = replace_na(as.character(CommonName), "No Common Name Assigned")) %>%
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
    
    plot <- sampleFDataRSD %>%
      plot_ly(
        x = ~RSD, 
        y = ~Count, 
        color = ~SpeciesCode, 
        type = 'bar', 
        text = ~hoverText,
        hoverinfo = 'text'
      ) %>%
      layout(
        barmode = 'stack', # Mimics geom_col() behavior
        title = "Length Frequencies",
        xaxis = list(title = "RSD (mm)"),
        yaxis = list(title = "Count"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
    
  } else{ 
    plot <- data %>%
      plot_ly(
        x = ~get(lengthOptions),
        weights = ~NumFish,
        color = ~SpeciesCode,
        type = "histogram",
        nbinsx = 30, # Adjust to match your previous 'binwidth'
        # Plotly calculates bins, so we use its internal hover variables
        # %{x}: Shows the bin range.
        # %{y}: Shows the summed NumFish for that bin.
        # <extra></extra>: Removes the annoying secondary "trace name" box that Plotly adds by default.
        hovertemplate = paste0(
          "Species: %{fullData.name}<br>",
          "Length Range: %{x}<br>",
          "Total Count: %{y}<extra></extra>"
        )
      ) %>%
      layout(
        barmode = "stack",
        title = "Length Frequencies",
        xaxis = list(title = paste(lengthOptions, "(Binwidth:", binwidth, ")")),
        yaxis = list(title = "Total Count"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )

  }

  return(plot)
}