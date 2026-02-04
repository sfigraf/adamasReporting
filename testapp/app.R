#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

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
             tabPanel(tags$div("SampleF Data", style = title_style), 
                      
                      sidebarPanel(
                        
                        virtualSelectInput(ns("areaBioSearch"),
                                           label = "Area Bio",
                                           choices = initialValues$allBios,
                                           multiple = TRUE,
                                           search = TRUE,          
                                           autoSelectFirstOption = FALSE, 
                                           #this ensures the dropdown is fully visible over the slider
                                           dropboxWrapper = "body",
                                           zIndex = 99999 
                        ), 
                        virtualSelectInput(ns("SpConBioSearch"),
                                           label = "Sp Con Bio",
                                           choices = initialValues$allSPBios,
                                           multiple = TRUE,
                                           search = TRUE,          
                                           autoSelectFirstOption = FALSE, 
                                           #this ensures the dropdown is fully visible over the slider
                                           dropboxWrapper = "body",
                                           zIndex = 99999
                        ), 
                        
                        virtualSelectInput(ns("waterNameSearch"),
                                           label = "Water Name",
                                           choices = initialValues$allDistinctWaters,
                                           multiple = TRUE,
                                           search = TRUE,          
                                           autoSelectFirstOption = FALSE, 
                                           #this ensures the dropdown is fully visible over the slider
                                           dropboxWrapper = "body",
                                           zIndex = 99999
                        ), 
                        
                        virtualSelectInput(ns("stationCodeSearch"),
                                           label = "Station Code",
                                           choices = sort(initialValues$allStationCodes),
                                           multiple = TRUE,
                                           search = TRUE,          
                                           autoSelectFirstOption = FALSE, 
                                           #this ensures the dropdown is fully visible over the slider
                                           dropboxWrapper = "body",
                                           zIndex = 99999
                        ), 
                        
                        virtualSelectInput(ns("surveyIDSearch"),
                                           label = "Survey ID",
                                           choices = sort(initialValues$allSurveyIDs),
                                           multiple = TRUE,
                                           search = TRUE,          
                                           autoSelectFirstOption = FALSE, 
                                           #this ensures the dropdown is fully visible over the slider
                                           dropboxWrapper = "body",
                                           zIndex = 99999
                        ), 
                        
                        uiOutput(ns("yearSliderUI")),
                        
                        checkboxInput(ns("lengthFilter"), "Display Length Filter"),
                        uiOutput(ns("lengthFilterUI")), 
                        
                        actionButton(ns("queryButton"), 
                                     label = "Render Data", width = "100%"), 
                        
                        h6("Note: entries with NA values in any of the filter fields are excluded from the results")
                        
                      ),
                      
                      mainPanel(
                        uiOutput(ns("mainPanelUI"))
                      )
                      
                      ),  
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
