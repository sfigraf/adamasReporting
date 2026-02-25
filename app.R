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
#library(bsicons) # for export icon, could try and use shiny icons instead

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
  
  #nned the first time visible reactiveVals becuase nothing is returned at first from the mods; once render buttons are clicked the the displayBUttons are activated
  firstTimesampleFButton <- reactiveVal(FALSE)
  sampleFDataDisplayButton <- reactiveVal(FALSE)
  samplFmenuVisible <- reactiveVal(FALSE)
  
  firstTimeSummarizedButton <- reactiveVal(FALSE)
  summarizedDataDisplayButton <- reactiveVal(FALSE)
  summarizedMenuVisible <- reactiveVal(FALSE)
  
  #data to download
  dataToDownload <- reactiveVal(NULL)
  
  observe({
    
    sampleFDataDisplayButton(sampleFModReturns$displayButton())
    summarizedDataDisplayButton(summarizedDataModReturns$displayButton())
    
    ###SAMPLE F
    #if no data has been queried and no data has been rendered once, then show nothing
    #if the button is pressed, then the display button will return positive, so show the 2 buttons
    if(input$main_nav == "sampleFDataTab" && !firstTimesampleFButton() && sampleFDataDisplayButton()){
      # if the other menu has been rendered before, remove it ifrst before render
      if(summarizedMenuVisible()){
        nav_remove("main_nav", target = "exportOptions")
        #update value to show summarized data mod buttons are not visible
        summarizedMenuVisible(FALSE)
      }
      ## insert buttons
          nav_insert(
            id = "main_nav",
            nav = nav_menu(
              title = span(bsicons::bs_icon("database-fill-down"), " Export"),
              value = "exportOptions",
              align = "right",
              nav_item(uiOutput("downloadDataUI", inline = TRUE)),
              nav_item(uiOutput("reportBuilderUI", inline = TRUE))

            )
          )
          
          #change reactive values
      samplFmenuVisible(TRUE)
      firstTimesampleFButton(TRUE)
      
      #if the sampleF display is on and the data has been rendered before and it's not currently showing and the server returns to display, then display again
    } else if(input$main_nav == "sampleFDataTab" && !samplFmenuVisible() && firstTimesampleFButton() && sampleFDataDisplayButton()) {
      #if summarized buttons already visiible, remove them
      if(summarizedMenuVisible()){
        nav_remove("main_nav", target = "exportOptions")
        #update value to show summarized data mod buttons are not visible
        summarizedMenuVisible(FALSE)
      }
      
      nav_insert(
        id = "main_nav",
        nav = nav_menu(
          title = span(icon("file-export"), " Export"),
          value = "exportOptions",
          align = "right",
          nav_item(uiOutput("downloadDataUI")),
          nav_item(uiOutput("reportBuilderUI"))
          
        )
      )
      
      #change reactive values to show menu is visible
      samplFmenuVisible(TRUE)
      
      #if the sampleF display buttons returns false, remove data download options
    } else if(input$main_nav == "sampleFDataTab" && !sampleFDataDisplayButton()){
      
      nav_remove("main_nav", target = "exportOptions")
      #update value to show sample F data mod buttons are not visible
      samplFmenuVisible(FALSE)
      
      ####Summarized Data
      #if tab is selected and no data has been rendered for it beofre, don't show options
      # if tab is selected and it's never been rendered before and the display button is on due to render click, then render the buttons
    } else if(input$main_nav == "currentSummariesDataTab" && !firstTimeSummarizedButton() && summarizedDataDisplayButton()){
      #if other menu is already on, remove it first
      if(samplFmenuVisible()){
        nav_remove("main_nav", target = "exportOptions")
        #update value to show sample f data mod buttons are not visible
        samplFmenuVisible(FALSE)
      }
      #insert buttons, just download button, not report
      nav_insert(
        id = "main_nav",
        nav = nav_menu(
          title = span(icon("file-export"), " Export"),
          value = "exportOptions",
          align = "right",
          nav_item(uiOutput("downloadDataUI"))
        )
      )
      #update reactive values
      firstTimeSummarizedButton(TRUE)
      summarizedMenuVisible(TRUE)
      
      # if it's the first time showing in the summarized tab but the query button hasn't been pressed yet, remove previosu menu
    } else if(input$main_nav == "currentSummariesDataTab" && !firstTimeSummarizedButton() && !summarizedDataDisplayButton()) {
      if(samplFmenuVisible()){
        nav_remove("main_nav", target = "exportOptions")
        #update value to show sample f data mod buttons are not visible
        samplFmenuVisible(FALSE)
      }
      #if it's not the first time rendering and it's not already showing 
    } else if(input$main_nav == "currentSummariesDataTab" && firstTimeSummarizedButton() && summarizedDataDisplayButton() && !summarizedMenuVisible()){
      #remove previous buttons if present
      if(samplFmenuVisible()){
        nav_remove("main_nav", target = "exportOptions")
        #update value to show sample f data mod buttons are not visible
        samplFmenuVisible(FALSE)
      }
      #insert buttons, just download button, not report
      nav_insert(
        id = "main_nav",
        nav = nav_menu(
          title = span(icon("file-export"), " Export"),
          value = "exportOptions",
          align = "right",
          nav_item(uiOutput("downloadDataUI"))
        )
      )
      
      summarizedMenuVisible(TRUE)
    }
  })
  
  #update data based on which tab is selected
  observe({ #input$main_nav, 
    #trigegers every time the "render" button is pressed in either mod since that's when values are returned from the servers
    
    if (input$main_nav == "sampleFDataTab") {
      dataToDownload(sampleFModReturns$data())

    } else if (input$main_nav == "currentSummariesDataTab") {
      dataToDownload(summarizedDataModReturns$data())
    }
  }) 
  
  output$downloadDataUI <- renderUI({
    #req(nrow(sampleFModReturns$data()) > 0)
    downloadData_UI("downloadAdamasData")
  })
  output$reportBuilderUI <- renderUI({
    req(nrow(sampleFModReturns$data()) > 0)
    runReport_UI("reportBuilder")
  })
  
  #sample f rawe data tab
  #not using sampleFDataList()$sampleFRawDataToDisplay because that unwraps the object and passes the static result of the data at that exact moment. instead, 
  #reactive({sampleFDataList()$sampleFRawDataToDisplay}) passes the reactive object itself and tells the mod to "go get" the data
  #same idea around making the filename reactive. one option is reactive({ paste0(input$waterNameSearch) })
  downloadData_Server("downloadAdamasData", reactive({dataToDownload()}),  "AdamasDataDownload")
  #currently report only exists for sample F so just returning this data
  runReport_Server("reportBuilder", reactive({sampleFModReturns$data()}), rsdLimits = rsdLimits)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
