###sampleFData
allDistinctWaterssql <- c("SELECT DISTINCT WaterName FROM SampleFView")
allDistinctWaters <- dbGetQuery(CPW_AqDatAnalysis, allDistinctWaterssql)


allYearssql <- c("SELECT DISTINCT year(SampleDate) FROM SampleFView")
allYears <- dbGetQuery(CPW_AqDatAnalysis, allYearssql)

allBios <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
  distinct(AreaBio) %>%
  show_query() %>%
  pull() 

allSPBios <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
  distinct(SpConBio) %>%
  show_query() %>%
  pull() 


sampleFData_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        
        virtualSelectInput(ns("areaBioSearch"),
                           label = "Area Bio",
                           choices = allBios,
                           multiple = TRUE,
                           search = TRUE,          
                           autoSelectFirstOption = FALSE, 
                           #this ensures the dropdown is fully visible over the slider
                           dropboxWrapper = "body" 
        ), 
        virtualSelectInput(ns("SpConBioSearch"),
                           label = "Sp Con Bio",
                           choices = allSPBios,
                           multiple = TRUE,
                           search = TRUE,          
                           autoSelectFirstOption = FALSE, 
                           #this ensures the dropdown is fully visible over the slider
                           dropboxWrapper = "body" 
        ), 
        
        virtualSelectInput(ns("waterNameSearch"),
          label = "Water Name",
          choices = allDistinctWaters,
          multiple = TRUE,
          search = TRUE,          
          autoSelectFirstOption = FALSE, 
          #this ensures the dropdown is fully visible over the slider
          dropboxWrapper = "body" 
        ), 
        # sliderInput(ns("yearSlider"), "Date",
        #             min = min(allYears, na.rm = TRUE),
        #             max = max(allYears, na.rm = TRUE),  
        #             value = c(min(allYears, na.rm = TRUE), max(allYears, na.rm = TRUE)),
        #             step = 1, 
        #             sep = ""
        #             #timeFormat = "%y"
        # ),
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
      
      inputsToListen <- reactive({
        list(input$waterNameSearch,
             input$areaBioSearch, 
             input$SpConBioSearch
             )
      })

# UI Components -----------------------------------------------------------
      
      
      output$yearSliderUI <- renderUI({
        if(isTruthy(input$waterNameSearch)){
          waterNames <- input$waterNameSearch
          
          selectedWatersOnly <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
            filter(WaterName %in% waterNames)
          allyears <- selectedWatersOnly %>%
            distinct(year(SampleDate)) %>%
            pull()
          # singleWaterOnly <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
          #   filter(WaterName == input$waterNameSearch)
          # allyears <- singleWaterOnly %>%
          #   distinct(year(SampleDate)) %>%
          #   pull()
          tagList(
            sliderInput(ns("yearSlider"), "Date",
                        min = min(allyears, na.rm = TRUE),
                        max = max(allyears, na.rm = TRUE),  
                        value = c(min(allyears, na.rm = TRUE), max(allyears, na.rm = TRUE)),
                        step = 1, 
                        sep = ""
                        #timeFormat = "%y"
            )
          )
          
        }
      })
        
      # observeEvent(inputsToListen(), {
      #   
      #   waterNames <- input$waterNameSearch
      # 
      #   selectedWatersOnly <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
      #     filter(WaterName %in% waterNames)
      #   allyears <- selectedWatersOnly %>%
      #     distinct(year(SampleDate)) %>%
      #     pull()
      #   
      #   updateSliderInput(
      #     session, 
      #     "yearSlider",
      #     min = min(allyears),
      #     max = max(allyears),  
      #     value = c(min(allyears), max(allyears))
      #   )
      #   
      # }, ignoreInit = TRUE)
      

# data wrangling ----------------------------------------------------------
      
      sampleFDataToDisplay <- eventReactive(input$queryButton,ignoreNULL = TRUE,{
        
        ##ERROR: Error in .transformer: `value` must be a string or scalar SQL, not the number 1. 
        #caused because it's hard to dbplyr to translate R to sql with lists directly inside a filter for a remote database table
        #making variables beforehand alllows us to to use them as "scalars" that it knows how to converrt to SQL values
        yearMin <- as.integer(input$yearSlider[1])
        yearMax <- as.integer(input$yearSlider[2])
        waterNames <- input$waterNameSearch
        
        data <- tbl(CPW_AqDatAnalysis, tableName) %>%
          filter(WaterName %in% waterNames, 
                 year(SampleDate) >= yearMin & year(SampleDate) <= yearMax
                 # AreaBio %in% input$areaBioSearch, 
                 # SpConBio %in% input$SpConBioSearch,
                 ) %>%
          as.data.frame()
        #data <- as.data.frame(data)
        return(data)
      })

# Output display ----------------------------------------------------------

      
      
      output$sampleFData <- renderDT({
        
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