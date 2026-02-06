###make summary tables function

getCombinedSummariesTables <- function(groupByCols, data){

  
  sampleFGrouped <- data %>%
    mutate(Year = year(SampleDate)) %>%
    group_by(across(any_of(groupByCols)))
  
  ### mean min max length and weight
  meanMinMaxLengthWeights <- sampleFGrouped %>%
    summarize(`Total Catch` = sum(NumFish), 
              `Average Length (mm)` = round(mean(Length_mm, na.rm = TRUE), 2), 
              `Median Length (mm)` = round(median(Length_mm, na.rm = TRUE), 2), 
              `Min Length (mm)` = round(min(Length_mm, na.rm = TRUE), 2),
              `Max Length (mm)` = round(max(Length_mm, na.rm = TRUE), 2), 
              `Standard Deviation (mm)` = round(sd(Length_mm, na.rm = TRUE), 2), 
              
              `Average Weight (g)` = round(mean(Weight_g, na.rm = TRUE), 2), 
              `Median Weight (g)` = round(median(Weight_g, na.rm = TRUE), 2), 
              `Min Weight (g)` = round(min(Weight_g, na.rm = TRUE), 2),
              `Max Weight (g)` = round(max(Weight_g, na.rm = TRUE), 2)
    )
  
  meanMinMaxLengthWeightsTable <- datatable(meanMinMaxLengthWeights,
                                            rownames = FALSE,
                                            extensions = c('Buttons'),
                                            #for slider filter instead of text input
                                            filter = 'top',
                                            options = list(
                                              pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                                              dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
                                              language = list(emptyTable = "Enter inputs and press Render Table")
                                            )
  )
  #### Porportional Stock Density Table
  proportionalstockdensity <- sampleFGrouped %>%
    summarize(`Total Catch` = sum(NumFish),
              #not entirely sure why using mean works but it does
              `Percent Stock size` = round(mean(RSD == "Stock", na.rm = TRUE) *100, 2), 
              `Percent Quality size` = round(mean(RSD == "Quality", na.rm = TRUE) *100, 2), 
              `Percent Preferred size` = round(mean(RSD == "Preferred", na.rm = TRUE) *100, 2), 
              `Percent Memorable size` = round(mean(RSD == "Memorable", na.rm = TRUE) *100, 2), 
              `Percent Trophy size` = round(mean(RSD == "Trophy", na.rm = TRUE) *100, 2), 
              `Max Length (mm)` = max(Length_mm, na.rm = TRUE)
    )
  proportionalstockdensityTable <- datatable(proportionalstockdensity,
                                             rownames = FALSE,
                                             extensions = c('Buttons'),
                                             filter = 'top',
                                             options = list(
                                               pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                                               dom = 'lfrtip', 
                                               language = list(emptyTable = "Enter inputs and press Render Table")
                                             )
  )
  
  
  #### relative abundace and CPUE
  relAbundanceCPUE <- sampleFGrouped %>%
    summarize(`Total Catch` = sum(NumFish), 
              `Weight Kg` = round(sum(Weight_g, na.rm = TRUE)/1000, 2), 
              #sums whole column numfish but ignores group_by()
              #could also do the same thing with mutating after
              `Percent Total Catch` = round(sum(NumFish, na.rm = TRUE)/sum(.$NumFish, na.rm = TRUE) *100, 2), 
              `Percent Total Weight` = round(sum(Weight_g, na.rm = TRUE)/sum(.$Weight_g, na.rm = TRUE) *100, 2), 
    )
  
  relAbundanceCPUETable <- datatable(relAbundanceCPUE,
                                     rownames = FALSE,
                                     extensions = c('Buttons'),
                                     filter = 'top',
                                     options = list(
                                       pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                                       dom = 'lfrtip', 
                                       language = list(emptyTable = "Enter inputs and press Render Table")
                                     )
  )
  DTList <- list(
    "meanMinMaxLengthWeightsTable" = meanMinMaxLengthWeightsTable,
    "proportionalstockdensityTable" = proportionalstockdensityTable,
    "relAbundanceCPUETable" = relAbundanceCPUETable
  )
  return(DTList)
}