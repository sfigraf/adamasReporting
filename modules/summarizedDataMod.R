summarizedData_UI <- function(id, initialValues) {
  ns <- NS(id)
  
  tagList(

    sidebarLayout(
      sidebarPanel(
        virtualSelectInput(ns("areaBioSearch"),
                           label = "Area Bio",
                           choices = initialValues$allBiosSummarizedData,
                           multiple = TRUE,
                           search = TRUE,          
                           autoSelectFirstOption = FALSE, 
                           #this ensures the dropdown is fully visible over the slider
                           dropboxWrapper = "body",
                           zIndex = 99999 
        ), 
        virtualSelectInput(ns("waterNameSearch"),
                           label = "Water Name",
                           choices = initialValues$allDistinctWatersSummarizedData,
                           multiple = TRUE,
                           search = TRUE,          
                           autoSelectFirstOption = FALSE, 
                           #this ensures the dropdown is fully visible over the slider
                           dropboxWrapper = "body", 
                           zIndex = 99999
        ), 
        sliderInput(ns("yearSlider"), "Date",
                    min = min(initialValues$allYearsSummarizedData, na.rm = TRUE),
                    max = max(initialValues$allYearsSummarizedData, na.rm = TRUE),  
                    value = c(min(initialValues$allYearsSummarizedData, na.rm = TRUE), max(initialValues$allYearsSummarizedData, na.rm = TRUE)),
                    step = 1, 
                    sep = ""
        ),
        actionButton(ns("queryButton"),
                     label = "Render Data", width = "100%", class = "btn-cpw-sidebar")

      ),

      mainPanel(
        uiOutput(ns("mainPanelUI"))
      )
    )
  )
}

summarizedData_Server <- function(id, currentSummaryDataAsTable, initialValues) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      

# UI Components -----------------------------------------------------------

      output$mainPanelUI <- renderUI({
        #only run this block when this button is clicked
        input$queryButton
        #do NOT re-run this block just becuase the values changed; wait for input$queryButton
        #yearInputCheck <- isolate(input$yearSlider)
        #if button hasn't been clicked at all yet, retun this message
        if (input$queryButton == 0 || nrow(currentSummaryDataToDisplay()) == 0) {
          return(p("Please select a valid area bio, Water Name, or year range and click 'Render'.", 
                   style = "color: gray;"))
        }

        #if we make it this far, it's becausse all the previosu conditions are met and we can successfully render the UI
        tagList(
          uiOutput(ns("downloadDataUI")),
          box(
            withSpinner(DTOutput(ns("currentSummariesData")))
          )
        )
      })
      # #save data option only appears if there's a valid dataset to download
      output$downloadDataUI <- renderUI({
        req(nrow(currentSummaryDataToDisplay()) > 0)
        downloadData_UI(ns("downloadcurrentSummariesData"))
      })
      
      #update watername based on inputs
      inputsToListen <- reactive({
        list(
          input$areaBioSearch
        )
      })

      observeEvent(inputsToListen(), {
        
        areaBios <- input$areaBioSearch
        #build query incrementally
        currentSummaryDataforWaterNames<- currentSummaryDataAsTable
        
        if(isTruthy(areaBios)){
          if(isTruthy(areaBios)){
            currentSummaryDataforWaterNames <- currentSummaryDataforWaterNames %>%
              #!! bang bang operator tells it to evaluate this statement instead of looking for a column named areaBios; not sure if 100% needed but ok
              filter(AreaBio %in% !!areaBios)
            
          }
          ##freeze these input reactive values and ignore downstream observers (in this case, the Slider UI render) until they have completely finisheed
          #this prevents slider from "re-rendering" once first for watername update and again for stationcode update
          #could also try looking into debounce() to wait a few milliseconds for the reactives to settle
          freezeReactiveValue(input, "waterNameSearch")
          
          #update waterNmaes based on bio selection
          selectedWaterNames <- currentSummaryDataforWaterNames %>%
            distinct(WaterName) %>%
            #show_query() %>%
            #collect() is when the query actually runs, just builds a query until then
            #returns as df
            collect() %>%
            #just pulls out the one column
            pull() %>%
            as.character() %>%
            sort()
          #error: in as.vector: cannot coerce type 'environment' to vector of type 'character' solved by explicitly making it a character. 
          #cleanChoices <- as.character(selectedWaterNames)
          updateVirtualSelect(
            session = session,
            "waterNameSearch", 
            choices = selectedWaterNames, 
            selected = selectedWaterNames
          )
          
        } else {
          updateVirtualSelect(
            session = session,
            "waterNameSearch", 
            choices = initialValues$allDistinctWatersSummarizedData
          )
        }
      }, ignoreInit = TRUE)
      
      observeEvent(input$waterNameSearch, {

        waterNames <- input$waterNameSearch
        areaBios <- input$areaBioSearch
        
        #build query incrementally
        currentSummaryDataForYears <- currentSummaryDataAsTable
        
        if(isTruthy(waterNames)){
          if(isTruthy(waterNames)){
            currentSummaryDataForYears <- currentSummaryDataForYears %>%
              filter(WaterName %in% waterNames
              )
            
          }
          if(isTruthy(areaBios)){
            currentSummaryDataForYears <- currentSummaryDataForYears %>%
              #!! bang bang operator tells it to evaluate this statement instead of looking for a column named areaBios; not sure if 100% needed but ok
              filter(AreaBio %in% !!areaBios)
            
          }
          
          #freezeReactiveValue(input, "yearSlider")
          #freezeReactiveValue(input, "waterNameSearch")
          
          
          #update waterNmaes based on bio selection
          selectedYears <- currentSummaryDataForYears %>%
            distinct(year(SampleDate)) %>%
            #show_query() %>%
            #collect() is when the query actually runs, just builds a query until then
            #returns as df
            collect() %>%
            #just pulls out the one column
            pull() 
          #unname()
          #error: in as.vector: cannot coerce type 'environment' to vector of type 'character' solved by explicitly making it a character. 
          #cleanChoices <- as.character(selectedWaterNames)
          updateSliderInput(
            session = session,
            "yearSlider", 
            min = min(selectedYears, na.rm = TRUE),
            max = max(selectedYears, na.rm = TRUE),  
            value = c(min(selectedYears, na.rm = TRUE), max(selectedYears, na.rm = TRUE))
          )
          
        } else {
          
          updateSliderInput(
            session = session,
            "yearSlider", 
            min = min(initialValues$allYearsSummarizedData, na.rm = TRUE),
            max = max(initialValues$allYearsSummarizedData, na.rm = TRUE),  
            value = c(min(initialValues$allYearsSummarizedData, na.rm = TRUE), max(initialValues$allYearsSummarizedData, na.rm = TRUE))
          )
        }
      }, ignoreInit = TRUE, ignoreNULL = FALSE) #ignoreNULL = FALSE means to react on an empty virtualSelect INput here
# Data Wrangling ----------------------------------------------------------

      
      currentSummaryDataToDisplay <- eventReactive(input$queryButton, ignoreNULL = TRUE,{
        
        yearMin <- as.integer(input$yearSlider[1])
        yearMax <- as.integer(input$yearSlider[2])
        waterNames <- input$waterNameSearch
        areaBios <- input$areaBioSearch
        
        currentSummaryDataToFilter <- currentSummaryDataAsTable %>%
          filter(year(SampleDate) >= yearMin & year(SampleDate) <= yearMax)
        
        if(isTruthy(waterNames)){
          currentSummaryDataToFilter <- currentSummaryDataToFilter %>%
            filter(WaterName %in% waterNames
            )
        }

        if(isTruthy(areaBios)){
          currentSummaryDataToFilter <- currentSummaryDataToFilter %>%
            #!! bang bang operator tells it to evaluate this statement instead of looking for a column named areaBios; not sure if 100% needed but ok
            filter(AreaBio %in% !!areaBios)

        }
        finalFilteredData <- currentSummaryDataToFilter %>%
          #show_query() %>%
          collect() 
        #columns in this db are "blobs" type which are found in DBs I guess. this converts them to character type and allows DT to display them
        finalFilteredData1 <- finalFilteredData %>%
          mutate(across(where(~inherits(., "blob")), 
                        ~sapply(., function(x) paste(as.character(x), collapse = ""))))

        return(finalFilteredData1)
      })
      
      output$currentSummariesData <- renderDT(server = TRUE, {
        
        datatable(currentSummaryDataToDisplay(),
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
      
      downloadData_Server("downloadcurrentSummariesData", currentSummaryDataToDisplay, "CurrentSummaryData")

    }
  )
}