##scratch paper

input <- list("waterNameSearch" == "HORSETOOTH RESERVOIR")
data <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
  filter(WaterName == input$waterNameSearch, 
         year(SampleDate)==input$yearsSearch)

singleWaterOnly <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
  filter(WaterName == "HORSETOOTH RESERVOIR")#, 
         #year(SampleDate) == 2025)
#dbplyr doesn't pull data into R unless explicitly asked so that's why it shows up as a list
show_query(singleWaterOnly)
allyears <- singleWaterOnly %>%
  distinct(year(SampleDate)) %>%
  pull()
#print(allyears)

years <- unique(year(singleWaterOnly$SampleDate))

x <- as.data.frame(singleWaterOnly)

data <- tbl(CPW_AqDatAnalysis, "CurrentSummary")
allYears <- data %>%
  distinct(year(SampleDate)) %>%
  pull()

x <- data %>%
  filter(year(SampleDate) == 2025) %>%
  as.data.frame() #%>%
  #select(1)
#library(blob)
blob_cols <- sapply(x, inherits, "blob")
x1 <- x %>%
  mutate(across(where(~inherits(., "blob")), 
                ~sapply(., function(x) paste(as.character(x), collapse = ""))))
#install.packages("blob")
datatable(x)
show_query(x)

x1 <- as.data.frame(saveDat)
datatable(x1)
