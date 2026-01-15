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
source("misc/graphicsOptions.R")

for (i in list.files("./modules/")) {
  if (grepl(".R", i)) {
    source(paste0("./modules/",i))
  }
}


# Define UI for application that draws a histogram
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
             # tabPanel("SampleF Data UI 2", 
             #          sampleFData_UI2("sampleFData2")), 
             tabPanel(tags$div("Summarized Data", style = title_style), 
                      summarizedData_UI("summarizedData"))
  )    

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  observe({
    sampleFData_Server("sampleFData", tableName = "SampleFView")
    #sampleFData_Server2("sampleFData2", tableName = "SampleFView")
    summarizedData_Server("summarizedData")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
