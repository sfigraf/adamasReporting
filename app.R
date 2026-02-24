library(odbc)
library(DBI)
library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet) #for map
library(sf)
library(plotly)
library(shinydashboard) #for box()
library(shinydashboardPlus)
library(readxl)
library(shinycssloaders) #withSpinner
library(DT)
library(shinyWidgets)
library(dbplyr)
library(openxlsx) #for saving excel file
library(knitr) #for rmarkdown
library(shinyjs) #for greying out buttons
#library(shinyvalidate)
library(later) #for watining to fire code
library(bslib) #for theme
library(sass)

source("scripts/connectToDB.R")
source("scripts/cssStuffbsLib.R")

for (i in list.files("./modules/")) {
  if (grepl(".R", i)) {
    source(paste0("./modules/",i))
  }
}

neededFunctions <- c("getCombinedSummariesTables.R", "getLengthWeightGraph.R", 
                     "getLengthFrequenciesGraph.R", "getRelativeWeightGraph.R")
for (i in neededFunctions) {
  source(paste0("./functions/",i))
}

##get initial Vlaues

###sampleFData options for filters
sampleFData <- tbl(pool, "SampleFView")

if(!exists("allDistinctWaters")){
  allDistinctWaters <- sampleFData %>%
    distinct(WaterName) %>%
    show_query() %>%
    pull() %>%
    sort()
}

if(!exists("allYears")){
  allYearssql <- c("SELECT DISTINCT year(SampleDate) FROM SampleFView")
  allYears <- dbGetQuery(pool, allYearssql)
}

if(!exists("allBios")){
  allBios <- sampleFData %>%
    distinct(AreaBio) %>%
    show_query() %>%
    pull() %>%
    sort()
}

if(!exists("allSPBios")){
  allSPBios <- sampleFData %>%
    distinct(SpConBio) %>%
    show_query() %>%
    pull() %>%
    sort()
}

if(!exists("allStationCodes")){
  allStationCodes <- sampleFData %>%
    distinct(StationCode) %>%
    show_query() %>%
    pull() %>%
    sort()
}

if(!exists("allSurveyIDs")){
  allSurveyIDs <- sampleFData %>%
    distinct(SurveyID) %>%
    show_query() %>%
    pull() %>%
    sort()
}

if(!exists("allLengths")){
  allLengths <- sampleFData %>%
    distinct(Length_mm) %>%
    show_query() %>%
    pull() 
}

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
currentSummaryData <- tbl(pool, "CurrentSummary")

if(!exists("allYearsSummarizedData")){
  allYearsSummarizedData <- currentSummaryData %>%
    distinct(year(SampleDate)) %>%
    pull()
}

if(!exists("allBiosSummarizedData")){
  allBiosSummarizedData <- currentSummaryData %>%
    distinct(AreaBio) %>%
    show_query() %>%
    pull() %>%
    sort()
}

if(!exists("allDistinctWatersSummarizedData")){
  allDistinctWatersSummarizedData <- currentSummaryData %>%
    distinct(WaterName) %>%
    show_query() %>%
    pull() %>%
    sort()
}

currentSummariesInitialFilterValues <- list(
  "allYearsSummarizedData" = allYearsSummarizedData, 
  "allBiosSummarizedData" = allBiosSummarizedData, 
  "allDistinctWatersSummarizedData" = allDistinctWatersSummarizedData
)
#rsd limits for graph rsd ranges
rsdLimits <- tbl(pool, "RSDLimitsView") %>%
  collect()

ui <- bslib::page_navbar(
  theme = my_theme,
  title = div(img(src="CPWLogoLarge.png", height = "60px", style = "margin-right: 15px;"), "Adamas Reporting"),
  #navbar_options = navbar_options(collapsible = FALSE),
  #selected = c("Map"),
  window_title = HTML("<title>Adamas Reporting</title> <link rel='icon' type='image/gif/png' href='CPWLogoLarge.png'>"),
  #this part changes the navbar options
  # header = tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "customStyles.css")
  # ),
  id = "main_nav",
  nav_menu("Data Source",
           nav_panel("Raw Data",
                     value = "sampleFDataTab",
                    sampleFData_UI("sampleFData", sampleFInitialFilterValues)),
           nav_panel("Aggregated Data",
                     value = "currentSummariesDataTab",
                    summarizedData_UI("summarizedData", currentSummariesInitialFilterValues))
           
           
  ),  
  nav_spacer() 
)

server <- function(input, output, session) {
  
  sampleFModReturns <- sampleFData_Server("sampleFData", sampleFData, sampleFInitialFilterValues, rsdLimits = rsdLimits)
  summarizedDataModReturns <- summarizedData_Server("summarizedData", currentSummaryData, currentSummariesInitialFilterValues)
  
  menu_visible <- reactiveVal(FALSE)
  
  dataToDownload <- reactiveVal(NULL)
  displayButton <- reactiveVal(FALSE)
  # dataExports <- reactiveValues(
  #   data = NULL, 
  #   displayButton = FALSE
  # )
  #update 
  observe({ #input$main_nav, 
    # print()
    print("inputobserved")
    if (input$main_nav == "sampleFDataTab") {
      print("sampleFtabSelected")
      dataToDownload(sampleFModReturns$data())
      displayButton(sampleFModReturns$displayButton())
      
    } else if (input$main_nav == "currentSummariesDataTab") {
      print("curretn summaries seleced")
      dataToDownload(summarizedDataModReturns$data())
      displayButton(summarizedDataModReturns$displayButton())
    }
    # print(nrow(dataExports$data))
    # print(dataExports$displayButton)
  }) 

  observe({
    #data <- sampleFModReturns$data() # Your module reactive
    # summarizedData <- summarizedDataModReturns$data()
    #print(paste("data exports display button:", dataExports$displayButton))
    print(paste("display in app.r", displayButton()))
    print(paste("display button value in app.r for summarizedData", summarizedDataModReturns$displayButton()))
    #print(paste("summarized data rows", nrow(summarizedData)))
    # Condition: data exists and menu hasn't been added yet
    if (displayButton() && !menu_visible()) { #
      
      nav_insert(
        id = "main_nav",
        # target = "testt", # Insert after the spacer
        # position = "after",
        nav = nav_menu(
          title = "Data export options",
          value = "exportOptions",
          align = "right",
          #nav_panel("heello", 
          nav_item(uiOutput("downloadDataUI")),
          nav_item(uiOutput("reportBuilderUI"))
          #)
          # div(style = "display: flex; gap: 10px; margin-bottom: 10px; margin-top: 10px;",
          #     ,
          
        )
        
        #)
      )
      
      menu_visible(TRUE) # Mark as added so it doesn't duplicate
      
    } else if (!displayButton()) { ##
      #print(paste("removed", x$displayButton()))
      # Optional: Remove the menu if data becomes empty again
      nav_remove("main_nav", target = "exportOptions")
      menu_visible(FALSE)
    }
  })
  
  output$downloadDataUI <- renderUI({
    req(nrow(sampleFModReturns$data()) > 0)
    downloadData_UI("downloadSampleFData")
  })
  output$reportBuilderUI <- renderUI({
    req(nrow(sampleFModReturns$data()) > 0)
    runReport_UI("reportBuilder")
  })
  
  #sample f rawe data tab
  downloadData_Server("downloadSampleFData", reactive({dataToDownload()}),  "AdamasDataDownload")
  runReport_Server("reportBuilder", reactive({sampleFModReturns$data()}), rsdLimits = rsdLimits)

    
  #observe({
  #})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
