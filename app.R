library(odbc)
library(DBI)
library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet) #for map
library(sf)
library(plotly)
library(shinydashboard) #for box()
library(readxl)
library(shinycssloaders) #withSpinner
library(DT)
library(shinyWidgets)
library(dbplyr)
library(openxlsx) #for saving excel file
library(knitr) #for rmarkdown
library(shinyjs) #for greying out buttons


source("scripts/connectToDB.R")
source("misc/graphicsOptions.R")

for (i in list.files("./modules/")) {
  if (grepl(".R", i)) {
    source(paste0("./modules/",i))
  }
}

##get initial Vlaues

###sampleFData options for filters
sampleFData <- tbl(CPW_AqDatAnalysis, "SampleFView")

allDistinctWaters <- sampleFData %>%
  distinct(WaterName) %>%
  show_query() %>%
  pull() %>%
  sort()

allYearssql <- c("SELECT DISTINCT year(SampleDate) FROM SampleFView")
allYears <- dbGetQuery(CPW_AqDatAnalysis, allYearssql)

allBios <- sampleFData %>%
  distinct(AreaBio) %>%
  show_query() %>%
  pull() %>%
  sort()

allSPBios <- sampleFData %>%
  distinct(SpConBio) %>%
  show_query() %>%
  pull() %>%
  sort()

allStationCodes <- sampleFData %>%
  distinct(StationCode) %>%
  show_query() %>%
  pull() %>%
  sort()

allSurveyIDs <- sampleFData %>%
  distinct(SurveyID) %>%
  show_query() %>%
  pull() %>%
  sort()

allLengths <- sampleFData %>%
  distinct(Length_mm) %>%
  show_query() %>%
  pull() 

sampleFInitialFilterValues <- list(
  "allBios" = allBios,
  "allSPBios" = allSPBios,
  "allDistinctWaters" = allDistinctWaters,
  "allStationCodes" = allStationCodes,
  "allSurveyIDs" = allSurveyIDs,
  "allYears" = allYears,
  "allLengths" = allLengths
)


###Same thing for current summaries
#current summary options for filters
currentSummaryData <- tbl(CPW_AqDatAnalysis, "CurrentSummary")
allYearsSummarizedData <- currentSummaryData %>%
  distinct(year(SampleDate)) %>%
  pull()
allBiosSummarizedData <- currentSummaryData %>%
  distinct(AreaBio) %>%
  show_query() %>%
  pull() %>%
  sort()
allDistinctWatersSummarizedData <- currentSummaryData %>%
  distinct(WaterName) %>%
  show_query() %>%
  pull() %>%
  sort()

currentSummariesInitialFilterValues <- list(
  "allYearsSummarizedData" = allYearsSummarizedData, 
  "allBiosSummarizedData" = allBiosSummarizedData, 
  "allDistinctWatersSummarizedData" = allDistinctWatersSummarizedData
)
#rsd limits for graph rsd ranges
rsdLimits <- tbl(CPW_AqDatAnalysis, "RSDLimitsView") %>%
  collect()

ui <- fluidPage(
  navbarPage(title = div(img(src="CPWLogoLarge.png", height = "60px", style = "margin-right: 15px;"), "Adamas Reporting"), 
             #selected = c("Map"),
             windowTitle = HTML("<title>Residual Pool Depth</title> <link rel='icon' type='image/gif/png' href='CPWLogoLarge.png'>"),
             #this part changes the navbar options
             tags$head(
               tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:9px !important; 
                            padding-bottom:0 !important;
                            height: 80px;
                            }
                           .navbar {min-height:25px !important;}'))
             ),
             id = "tabs", 
             theme = shinytheme("cerulean"),
             tabPanel(tags$div("SampleF Data",style = title_style), 
                      sampleFData_UI("sampleFData", sampleFInitialFilterValues)),  
             tabPanel(tags$div("Summarized Data", style = title_style), 
                      summarizedData_UI("summarizedData", currentSummariesInitialFilterValues))
  )    

)

server <- function(input, output) {
  observe({
    sampleFData_Server("sampleFData", sampleFData, sampleFInitialFilterValues, rsdLimits = rsdLimits)
    summarizedData_Server("summarizedData", currentSummaryData, currentSummariesInitialFilterValues)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
