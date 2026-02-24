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
                    sampleFData_UI("sampleFData", sampleFInitialFilterValues)),
           nav_panel("Aggregated Data",
                    summarizedData_UI("summarizedData", currentSummariesInitialFilterValues))
           
           
  ), 
  nav_menu("test", 
           nav_panel("test apnel", 
                     value = "testt",
                     sliderInput("Test", "test", 
                                 min = 1, max = 10, value = 2), 
                     actionButton("Testbt", "button"))
           ), 
  nav_spacer() 
)

server <- function(input, output, session) {
  
  x <- sampleFData_Server("sampleFData", sampleFData, sampleFInitialFilterValues, rsdLimits = rsdLimits)
  menu_visible <- reactiveVal(FALSE)
  
  # observeEvent(input$Testbt, {
  #   nav_insert()
  # })
  observe({
    data <- x$data() # Your module reactive
    print(paste("display button value in app.r", x$displayButton()))
    print(nrow(data))
    # Condition: data exists and menu hasn't been added yet
    if (x$displayButton() && !menu_visible()) { #
      
      nav_insert(
        id = "main_nav",
        # target = "testt", # Insert after the spacer
        # position = "after",
        nav = nav_menu(
          title = "Data export options",
          value = "exportOptions",
          align = "right",
          #nav_panel("heello", 
            nav_item(actionButton("test1", "Export CSV")),
            nav_item(actionButton("test2", "Export Excel"))
          #)
          
        )
      )
      
      menu_visible(TRUE) # Mark as added so it doesn't duplicate
      
    } else if (!x$displayButton()) { ##
      print(paste("removed", x$displayButton()))
      # Optional: Remove the menu if data becomes empty again
      nav_remove("main_nav", target = "exportOptions")
      menu_visible(FALSE)
    }
  })

    
  observe({
    summarizedData_Server("summarizedData", currentSummaryData, currentSummariesInitialFilterValues)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
