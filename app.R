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

source("scripts/connectToDB.R")

for (i in list.files("./modules/")) {
  if (grepl(".R", i)) {
    source(paste0("./modules/",i))
  }
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(title = "Adamas Reporting",
             id = "tabs", 
             theme = shinytheme("cerulean"),
             tabPanel("SampleF Data", 
                      sampleFData_UI("sampleFData")), 
             tabPanel("Summarized Data", 
                      summarizedData_UI("summarizedData"))
  )    

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observe({
    sampleFData_Server("sampleFData", tableName = "SampleFView")
    summarizedData_Server("summarizedData")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
