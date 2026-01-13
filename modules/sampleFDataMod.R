###sampleFData
allDistinctWaterssql <- c("SELECT DISTINCT WaterName FROM SampleFView")
allDistinctWaters <- dbGetQuery(CPW_AqDatAnalysis, allDistinctWaterssql)


# allYearssql <- c("SELECT DISTINCT year(SampleDate) FROM SampleFView")
# allYears <- dbGetQuery(CPW_AqDatAnalysis, allYearssql)

sampleFData_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        
        virtualSelectInput(ns("waterNameSearch"),
          label = "Water Name:",
          choices = allDistinctWaters,
          search = TRUE,          
          autoSelectFirstOption = FALSE
        ), 
        uiOutput(ns("yearSliderUI")),
        
        
        actionButton(ns("queryButton"), 
                     label = "Render Data", width = "100%")
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        DTOutput(ns("sampleFData")), 
        downloadData_UI(ns("downloadSampleFData"))
      )
    )
  
  )
}

sampleFData_Server <- function(id, tableName) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

# UI Components -----------------------------------------------------------
  output$yearSliderUI <- renderUI({
    if(isTruthy(input$waterNameSearch)){
      singleWaterOnly <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
        filter(WaterName == input$waterNameSearch)
      allyears <- singleWaterOnly %>%
        distinct(year(SampleDate)) %>%
        pull()
      print(class(allyears))
      print(min(allyears))
      tagList(
        sliderInput(ns("yearSlider"), "Date",
                    min = min(allyears),
                    max = max(allyears),  
                    value = c(min(allyears), max(allyears))#,
                    #step = 1,
                    #timeFormat = "%y"
        )
        # virtualSelectInput(ns("yearsSearch"),
        #                    label = "Year:",
        #                    choices = sort(allyears),
        #                    search = TRUE,     
        #                    autoSelectFirstOption = FALSE
        # )
      )
      
    }
  })

# data wrangling ----------------------------------------------------------
      
      sampleFDataToDisplay <- eventReactive(input$queryButton,ignoreNULL = TRUE,{
        year_start <- as.integer(input$yearSlider[1])
        year_end   <- as.integer(input$yearSlider[2])
        
        data <- tbl(CPW_AqDatAnalysis, tableName) %>%
          filter(WaterName == input$waterNameSearch, 
                 year(SampleDate) >= year_start & year(SampleDate) <= year_end
                 ) %>%
          as.data.frame()
        #data <- as.data.frame(data)
        return(data)
      })

# Output display ----------------------------------------------------------

      
      
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
      
      downloadData_Server("downloadSampleFData", sampleFDataToDisplay(), paste0(input$waterNameSearch))
      
    }
  )
}