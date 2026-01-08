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
#install.packages("dbplyr")

source("connectToDB.R")

allDistinctWaterssql <- c("SELECT DISTINCT WaterName FROM SampleFView")
allDistinctWaters <- dbGetQuery(CPW_AqDatAnalysis, allDistinctWaterssql)

allYearssql <- c("SELECT DISTINCT year(SampleDate) FROM SampleFView")
allYears <- dbGetQuery(CPW_AqDatAnalysis, allYearssql)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Adamas Reporting"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            virtualSelectInput(
              inputId = "waterNameSearch",
              label = "Water Name:",
              choices = allDistinctWaters,
              search = TRUE,          # Enables the search bar
              autoSelectFirstOption = FALSE
            ), 
            virtualSelectInput(
              inputId = "yearsSearch",
              label = "Year:",
              choices = allYears,
              search = TRUE,          # Enables the search bar
              autoSelectFirstOption = FALSE
            ), 
            
            actionButton("queryButton", label = "Render Data", width = "100%")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           DTOutput("sampleFData")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  sampleFDataToDisplay <- eventReactive(input$queryButton,ignoreNULL = TRUE,{
    data <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
      filter(WaterName == input$waterNameSearch, 
             year(SampleDate)==input$yearsSearch)
    # query <- paste0(c("Select * from SampleFView where year(SampleDate)=", input$yearsSearch, " and WaterName='", input$waterNameSearch, "'"))
    # print(query)
    # data <- dbGetQuery(CPW_AqDatAnalysis, query)
    data <- as.data.frame(data)
    return(data)
  })
  
  output$sampleFData <- renderDT({
    # detectionDataNoSF <- allDataFiltered()$detectionCountDataToDisplay #%>%
    #   #st_drop_geometry()
    datatable(sampleFDataToDisplay(),
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
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
