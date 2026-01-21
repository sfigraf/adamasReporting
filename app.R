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
                      sampleFData_UI("sampleFData")),  
             tabPanel(tags$div("Summarized Data", style = title_style), 
                      summarizedData_UI("summarizedData"))
  )    

)

server <- function(input, output) {
  observe({
    sampleFData_Server("sampleFData", tableName = "SampleFView")
    summarizedData_Server("summarizedData", tableName = "CurrentSummary")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
