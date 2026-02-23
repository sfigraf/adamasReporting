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
  #selected = c("Map"),
  window_title = HTML("<title>Adamas Reporting</title> <link rel='icon' type='image/gif/png' href='CPWLogoLarge.png'>"),
  #this part changes the navbar options
  # header = tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "customStyles.css")
  # ),
  id = "tabs",
  nav_menu("Data Source",
           nav_panel("Raw Data",
                    sampleFData_UI("sampleFData", sampleFInitialFilterValues)),
           nav_panel("Aggregated Data",
                    summarizedData_UI("summarizedData", currentSummariesInitialFilterValues))
           
           
  ), 
  nav_menu("test", 
           nav_panel("test apnel", 
                     sliderInput("Test", "test", 
                                 min = 1, max = 10, value = 2))
           )
)
#   fluidPage(
#   
#   
# )
  #dashboardPage(
  # dashboardHeader(title = "Adamas Reporting",
  #                 leftUi = tagList(
  #                   dropdownBlock(
  #                     id = "mydropdown",
  #                     title = "Data Source",
  #                     actionButton("btn1", "Raw Data"), 
  #                     actionButton("btn2", "summarized Data")
  #                     # 
  #                     # type = "messages",
  #                     # icon = icon("database"),
  #                     # headerText = "Data Source",
  #                     # # shinydashboardPlus adds 'inputId' to these items
  #                     # messageItem(from = "Raw", message = "View data", icon = icon("table"), inputId = "btn_raw"),
  #                     # messageItem(from = "Summary", message = "View summary", icon = icon("chart-line"), inputId = "btn_agg")
  #                   )
  #                 )
  # ), 
  # dashboardSidebar(
  #   width = 300,
  #   sidebarMenu(
  #     id = "sidebar",
  #     menuItem("Data Source", tabName = "dashboard", icon = icon("tachometer-alt")),
  #     menuItem("Projects", tabName = "projects", icon = icon("folder-open"))
  #     # menuItem("Data Editor", tabName = "editor", icon = icon("edit")),
  #     # menuItem("My Changes", tabName = "mychanges", icon = icon("history")),
  #     # conditionalPanel(
  #     #   condition = "output.is_admin == true",
  #     #   menuItem("Change Requests", tabName = "changes", icon = icon("inbox")),
  #     #   menuItem("User Management", tabName = "users", icon = icon("users"))
  #     # )
  #   )
  #  # hr(),
  #   # div(style = "padding: 10px; text-align: center; color: #9d9d9d;",
  #   #     p(style = "margin: 0;", "LOCAL DEMO VERSION"),
  #   #     p(style = "margin: 0; font-size: 12px;", "Wildlife Data Management"),
  #   #     br(),
  #   #     div(style = "background-color: #f39c12; color: white; padding: 5px; border-radius: 3px;",
  #   #         p(style = "margin: 0; font-size: 11px;", "⚠️ Using Mock Data")
  #   #     )
  #   # )
  # ),
  
  # body = dashboardBody(
  #   tags$head(
  #     tags$link(rel = "stylesheet", type = "text/css", href = "customStyles.css")
  #   ),
  #   tabItems(
  #     tabItem(tabName = "dashboard",
  #             sampleFData_UI("sampleFData", sampleFInitialFilterValues)
  #             ),
  #     tabItem(tabName = "projects", 
  #             summarizedData_UI("summarizedData", currentSummariesInitialFilterValues)
  #             )
  #   )
  # )
  # dashboardBody(
  #   
  #   tabItems(
  #     tabItem(tabName = "home", h2("Welcome! Load data to see options.")),
  #     
  #     # These are the targets for your dropdown buttons
  #     tabItem(tabName = "raw_data", 
  #             sampleFData_UI("sampleFData", sampleFInitialFilterValues)),  
  #     tabItem(tabName = "agg_data", 
  #             summarizedData_UI("summarizedData", currentSummariesInitialFilterValues))
  #   )
  # )
#)


server <- function(input, output, session) {
  # observeEvent(input$btn_raw, { 
  #   updateTabItems(session, "tabs", "raw_page") 
  #   sampleFData_Server("sampleFData", sampleFData, sampleFInitialFilterValues, rsdLimits = rsdLimits)
  #   
  #   })
  # observeEvent(input$btn_agg, { updateTabItems(session, "tabs", "agg_page") })
  # output$dataSourceMenu <- renderMenu({
  #   # req(data_is_loaded()) # Uncomment this to hide until data exists
  #   
  #   dropdownMenu(
  #     type = "messages", # Use 'messages' or 'tasks' for custom icons
  #     headerText = "Data Source",
  #     icon = icon("database"),
  #     
  #     # Custom message items that act as buttons
  #     messageItem(
  #       from = "Raw Data",
  #       message = "View unedited records",
  #       icon = icon("table"),
  #       href = "#", # Keeps it from refreshing the page
  #     ),
  #     messageItem(
  #       from = "Aggregated Data",
  #       message = "View summarized trends",
  #       icon = icon("chart-line"),
  #       href = "#"
  #     )
  #   )
  # })
    
  observe({
    sampleFData_Server("sampleFData", sampleFData, sampleFInitialFilterValues, rsdLimits = rsdLimits)
    summarizedData_Server("summarizedData", currentSummaryData, currentSummariesInitialFilterValues)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
