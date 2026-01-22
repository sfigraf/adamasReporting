sampleFData_UI <- function(id) {
  ns <- NS(id)
  
  ###sampleFData options for filters
  sampleFData <- tbl(CPW_AqDatAnalysis, "SampleFView")
  
  allDistinctWaters <- sampleFData %>%
    distinct(WaterName) %>%
    show_query() %>%
    pull() %>%
    sort()
  
  allYearssql <- c("SELECT DISTINCT year(SampleDate) FROM SampleFView")
  allYears <- dbGetQuery(CPW_AqDatAnalysis, allYearssql)
  
  allBios <- sampleFData %>%
    distinct(AreaBio) %>%
    show_query() %>%
    pull() %>%
    sort()
  
  allSPBios <- sampleFData %>%
    distinct(SpConBio) %>%
    show_query() %>%
    pull() %>%
    sort()
  
  allStationCodes <- sampleFData %>%
    distinct(StationCode) %>%
    show_query() %>%
    pull() %>%
    sort()
  
  allLengths <- sampleFData %>%
    distinct(Length_mm) %>%
    show_query() %>%
    pull() 
  
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
                           dropboxWrapper = "body",
                           zIndex = 99999 
        ), 
        virtualSelectInput(ns("SpConBioSearch"),
                           label = "Sp Con Bio",
                           choices = allSPBios,
                           multiple = TRUE,
                           search = TRUE,          
                           autoSelectFirstOption = FALSE, 
                           #this ensures the dropdown is fully visible over the slider
                           dropboxWrapper = "body",
                           zIndex = 99999
        ), 
        
        virtualSelectInput(ns("waterNameSearch"),
          label = "Water Name",
          choices = allDistinctWaters,
          multiple = TRUE,
          search = TRUE,          
          autoSelectFirstOption = FALSE, 
          #this ensures the dropdown is fully visible over the slider
          dropboxWrapper = "body",
          zIndex = 99999
        ), 
        
        virtualSelectInput(ns("stationCodeSearch"),
                           label = "Station Code",
                           choices = sort(allStationCodes),
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
    )
  
  )
}

sampleFData_Server <- function(id, tableName) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

# UI Components -----------------------------------------------------------
      
      # if sp bio or area bio updates, I want the waterNames and station codes to update
      inputsToListen <- reactive({
        list(
          input$areaBioSearch,
          input$SpConBioSearch
        )
      })
      
      observeEvent(inputsToListen(), {
        
        areaBios <- input$areaBioSearch
        spConBios <- input$SpConBioSearch
        #build query incrementally
        table <- tbl(CPW_AqDatAnalysis, "SampleFView")
        
        #if there is anything selected in either areaBios or Specis bios, udpate waternames and station code options
        #else, just go back to default options
        if(isTruthy(areaBios) || isTruthy(spConBios)){
          if(isTruthy(areaBios)){
            table <- table %>%
              #!! bang bang operator tells it to evaluate this statement instead of looking for a column named areaBios; not sure if 100% needed but ok
              filter(AreaBio %in% !!areaBios)
            
          }#allows it so query builds like a AND statement
          if(isTruthy(spConBios)) {
            table <- table %>%
              filter(SpConBio %in% !!spConBios)
          } 
          
          ##freeze these input reactive values and ignore downstream observers (in this case, the Slider UI render) until they have completely finisheed
          #this prevents slider from "re-rendering" once first for watername update and again for stationcode update
          #could also try looking into debounce() to wait a few milliseconds for the reactives to settle
          #from shiny website:  it’s actually good practice to always use it when you dynamically change an input value. The actual modification takes some time to flow to the browser then back to Shiny, and in the interim any reads of the value are at best wasted, and at worst lead to errors. Use freezeReactiveValue() to tell all downstream calculations that an input value is stale and they should save their effort until it’s useful.
          #https://mastering-shiny.org/action-dynamic.html
          freezeReactiveValue(input, "waterNameSearch")
          freezeReactiveValue(input, "stationCodeSearch")
          freezeReactiveValue(input, "lengthSlider")
          
          #update waterNmaes based on bio selection
          selectedWaterNames <- table %>%
            distinct(WaterName) %>%
            #show_query() %>%
            #collect() is when the query actually runs, just builds a query until then
            #returns as df
            collect() %>%
            #just pulls out the one column
            pull() %>%
            sort() %>%
            as.character()
          #error: in as.vector: cannot coerce type 'environment' to vector of type 'character' solved by explicitly making it a character. 
          #cleanChoices <- as.character(selectedWaterNames)
          updateVirtualSelect(
            session = session,
            "waterNameSearch", 
            choices = selectedWaterNames, 
            selected = selectedWaterNames
          )
          
          #update station codes based on bio selection
          selectedStationCodes <- table %>%
            distinct(StationCode) %>%
            #show_query() %>%
            #collect() is when the query actually runs, just builds a query until then
            #returns as df
            collect() %>%
            #just pulls out the one column
            pull() %>%
            sort() %>%
            as.character()
          
          
          updateVirtualSelect(
            session = session,
            "stationCodeSearch", 
            choices = sort(selectedStationCodes), 
            selected = selectedStationCodes
          ) 
          
        } else {
          updateVirtualSelect(
            session = session,
            "waterNameSearch", 
            choices = allDistinctWaters
          )
          
          updateVirtualSelect(
            session = session,
            "stationCodeSearch", 
            choices = allStationCodes
          )
        }
        
      }, ignoreInit = TRUE) #not sure why i don't need ignoreNull here and it works but whatever
      
      #year slider renders and updates with changes to each of the waterNames or station codes
      #waternames an changes based on sp con bio or area bio above
      
      output$yearSliderUI <- renderUI({
        #\|| means that second element will be only be evaluated if first isn't true; not sure if it matters here but probably speeds it up a tad
        if(isTruthy(input$waterNameSearch) || isTruthy(input$stationCodeSearch)) { #|| isTruthy(input$SpConBioSearch) #|| isTruthy(input$areaBioSearch)
          #update years based on waterName,
          waterNames <- input$waterNameSearch
          stationCodes <- input$stationCodeSearch

          sampleFForSlider <- tbl(CPW_AqDatAnalysis, "SampleFView")

          if(isTruthy(waterNames)){
            sampleFForSlider <- sampleFForSlider %>%
              filter(WaterName %in% waterNames)
          }

          if(isTruthy(stationCodes)){
            sampleFForSlider <- sampleFForSlider %>%
              filter(StationCode %in% stationCodes)
          }

          allyears <- sampleFForSlider %>%
            distinct(year(SampleDate)) %>%
            #show_query() %>%
            pull()

          tagList(
            sliderInput(ns("yearSlider"), "Date",
                        min = min(allyears, na.rm = TRUE),
                        max = max(allyears, na.rm = TRUE),
                        value = c(min(allyears, na.rm = TRUE), max(allyears, na.rm = TRUE)),
                        step = 1,
                        sep = ""
            )
          )
        }
      })
      
      #adding optional length filter that updates based off year slider, water names, and station code
      output$lengthFilterUI <- renderUI({
        
        req(input$lengthFilter)
        
        if(isTruthy(input$waterNameSearch) || isTruthy(input$stationCodeSearch)) { #|| isTruthy(input$SpConBioSearch) #|| isTruthy(input$areaBioSearch) 
          
          yearMin <- as.integer(input$yearSlider[1])
          yearMax <- as.integer(input$yearSlider[2])
          
          #update years based on waterName,
          waterNames <- input$waterNameSearch
          stationCodes <- input$stationCodeSearch
          
          sampleFForLengthSlider <- tbl(CPW_AqDatAnalysis, "SampleFView")
          
          if(isTruthy(waterNames)){
            sampleFForLengthSlider <- sampleFForLengthSlider %>%
              filter(WaterName %in% waterNames)
          }
          
          if(isTruthy(stationCodes)){
            sampleFForLengthSlider <- sampleFForLengthSlider %>%
              filter(StationCode %in% stationCodes)
          }
          #filter based off selected years as well
          #wait until year slider is valid before running this part
          req(all(is.numeric(input$yearSlider)))
          lengthListOptions <- sampleFForLengthSlider %>%
            filter(year(SampleDate) >= yearMin & year(SampleDate) <= yearMax) %>%
            distinct(Length_mm) %>%
            #show_query() %>%
            pull()
        } else {
          lengthListOptions <- allLengths
        }
        tagList(
          
          h6("Note: adding this filter automatically removes observations of fish with NA length"),
          suppressWarnings({
            sliderInput(ns("lengthSlider"), "Length (mm)",
                        min = min(lengthListOptions, na.rm = TRUE),
                        max = max(lengthListOptions, na.rm = TRUE),  
                        value = c(min(lengthListOptions, na.rm = TRUE), max(lengthListOptions, na.rm = TRUE)),
                        step = 1
            )
          })
        )
        
      })
      
      ####render mainpanel UI when the query button is clicked
      #if the inputs aren't valid then send a message
      #otherwise, render datatable and save button
      # INPUTS NEED TO BE WRAPPED IN isolate() or else it will alwys render when an input is called
      
      output$mainPanelUI <- renderUI({
        #only run this block when this button is clicked
        input$queryButton
        #do NOT re-run this block just becuase the values changed; wait for input$queryButton
        waterNameInputCheck <- isolate(isTruthy(input$waterNameSearch))
        stationCodeInputCheck <- isolate(isTruthy(input$stationCodeSearch))
        lengthInputCheck <- isolate(all(is.numeric(input$lengthSlider)))

        #if button hasn't been clicked at all yet, retun this message
        if (input$queryButton == 0) {
          return(p("Please select a Area Bio, Species Con Bio, Water Name, or Station Code and click 'Render'.", 
                   style = "color: gray;"))
        }
        #check if waterNames or Station Code inputs are valid, and return a message if not
        if (!(waterNameInputCheck || stationCodeInputCheck)) {
          return(p("Please select a Water Name or Station Code before rendering.", 
                   style = "color: gray;"))
        }
        #pretty much every time an input is called explicitly it should be wrapped in isolate() within this block to prevent UI render before input$querybutton is clicked
        if (isolate(input$lengthFilter) & !(lengthInputCheck)) {
          return(p("No data collected for selected year(s) or only NA lengths detected at this water. Please turn off length filter before rendering this data.",
                   style = "color: gray;"))
        }
        #if we make it this far, it's becausse all the previosu conditions are met and we can successfully render the UI
        tagList(
          tabsetPanel(
            tabPanel("Raw Data", 
                     div(style = "display: flex; gap: 10px; margin-bottom: 10px; margin-top: 10px;",
                         uiOutput(ns("downloadDataUI")),
                         uiOutput(ns("reportBuilderUI"))
                     ),
                     box(
                       withSpinner(DTOutput(ns("sampleFData")))
                     )
            ), 
            tabPanel("Species Summarized Data", 
                     div(style = "display: flex; gap: 10px; margin-bottom: 10px; margin-top: 10px;",
                         uiOutput(ns("downloadSummarizedDataUI"))
                     ),
                     box(
                       withSpinner(DTOutput(ns("sampleFSummarizedData")))
                     )
            )
          )
          
        )
      })
      # #save data option and run report options only appears if there's a valid dataset to download
      output$downloadDataUI <- renderUI({
        req(nrow(sampleFDataList()$sampleFRawDataToDisplay) > 0)
        downloadData_UI(ns("downloadSampleFData"))
      })
      output$reportBuilderUI <- renderUI({
        req(nrow(sampleFDataList()$sampleFRawDataToDisplay) > 0)
        runReport_UI(ns("reportBuilder"))
      })
      #for summarized data
      output$downloadSummarizedDataUI <- renderUI({
        req(nrow(sampleFDataList()$sampleFSummarizedData) > 0)
        downloadData_UI(ns("downloadSampleFSummarizedData"))
      })

# data wrangling ----------------------------------------------------------
      
      sampleFDataList <- eventReactive(input$queryButton, ignoreNULL = TRUE, {
        
        #only run if one of these are true. if not, it will get get caught in the render UI above
        req(isTruthy(input$waterNameSearch) || isTruthy(input$stationCodeSearch))

        ##ERROR: Error in .transformer: `value` must be a string or scalar SQL, not the number 1. 
        #caused because it's hard to dbplyr to translate R to sql with lists directly inside a filter for a remote database table
        #making variables beforehand alllows us to to use them as "scalars" that it knows how to converrt to SQL values
        yearMin <- as.integer(input$yearSlider[1])
        yearMax <- as.integer(input$yearSlider[2])
        waterNames <- input$waterNameSearch
        stationCodes <- input$stationCodeSearch
        areaBios <- input$areaBioSearch
        spConBios <- input$SpConBioSearch
        lengthMin <- as.integer(input$lengthSlider[1])
        lengthMax <- as.integer(input$lengthSlider[2])
        
        #filter data based on inputs
        #allows it so query builds like a AND statement
        samplFDataFiltered <- tbl(CPW_AqDatAnalysis, tableName) %>%
          filter(year(SampleDate) >= yearMin & year(SampleDate) <= yearMax)
        
        if(isTruthy(waterNames)){
          samplFDataFiltered <- samplFDataFiltered %>%
            filter(WaterName %in% waterNames) 
        }
        
        if(isTruthy(stationCodes)){
          samplFDataFiltered <- samplFDataFiltered %>%
            filter(StationCode %in% stationCodes) 
        }
          
        if(isTruthy(areaBios)){
          samplFDataFiltered <- samplFDataFiltered %>%
            #!! bang bang operator tells it to evaluate this statement instead of looking for a column named areaBios; not sure if 100% needed but ok
            filter(AreaBio %in% !!areaBios)
        }
        
        if(isTruthy(spConBios)) {
          samplFDataFiltered <- samplFDataFiltered %>%
            filter(SpConBio %in% !!spConBios)
        } 
        #if checkbox clicked (aka true) and inputs are valid numeric use length filter slider
        if(input$lengthFilter && all(is.numeric(input$lengthSlider))) {
          samplFDataFiltered <- samplFDataFiltered %>%
            filter(Length_mm >= lengthMin & Length_mm <= lengthMax)
        } 
        
        finalFilteredData <- samplFDataFiltered %>%
          #show_query() %>%
          collect()
        
        sampleFSummarizedData <- finalFilteredData %>%
          group_by(CommonName) %>%
          summarize(`Number of Fish` = n(), 
                    `Average Length (mm)` = round(mean(Length_mm, na.rm = TRUE), 2), 
                    `Median Length (mm)` = round(median(Length_mm, na.rm = TRUE), 2), 
                    `Min Length (mm)` = round(min(Length_mm, na.rm = TRUE), 2),
                    `Max Length (mm)` = round(max(Length_mm, na.rm = TRUE), 2), 
                    `Standard Deviation (mm)` = round(sd(Length_mm, na.rm = TRUE), 2)
                    )
        
        finalFilteredDataList <- list(
          "sampleFRawDataToDisplay" = finalFilteredData, 
          "sampleFSummarizedData" = sampleFSummarizedData
        )
        
        return(finalFilteredDataList)
        
      })

# Output display ----------------------------------------------------------
      
      output$sampleFData <- renderDT({
        
        req(sampleFDataList()$sampleFRawDataToDisplay)
        
        datatable(sampleFDataList()$sampleFRawDataToDisplay,
                  rownames = FALSE,
                  extensions = c('Buttons'),
                  #for slider filter instead of text input
                  filter = 'top',
                  options = list(
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
                    language = list(emptyTable = "Enter inputs and press Render Table")
                    #buttons = c('csv', 'excel')
                  )
        )
       
      }, server = TRUE)
      
      output$sampleFSummarizedData <- renderDT({
        
        req(sampleFDataList()$sampleFSummarizedData)
        
        datatable(sampleFDataList()$sampleFSummarizedData,
                  rownames = FALSE,
                  extensions = c('Buttons'),
                  #for slider filter instead of text input
                  filter = 'top',
                  options = list(
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
                    language = list(emptyTable = "Enter inputs and press Render Table")
                    #buttons = c('csv', 'excel')
                  )
        )
        
      }, server = TRUE)
      
      #not using sampleFDataList()$sampleFRawDataToDisplay because that unwraps the object and passes the static result of the data at that exact moment. instead, 
      #reactive({sampleFDataList()$sampleFRawDataToDisplay}) passes the reactive object itself and tells the mod to "go get" the data
      #same idea around making the filename reactive. one option is reactive({ paste0(input$waterNameSearch) })
      
      #sample f rawe data tab
      downloadData_Server("downloadSampleFData", reactive({sampleFDataList()$sampleFRawDataToDisplay}),  "SampleFData")
      runReport_Server("reportBuilder", reactive({sampleFDataList()$sampleFRawDataToDisplay}))
      #summarized data tab
      downloadData_Server("downloadSampleFSummarizedData", reactive({sampleFDataList()$sampleFSummarizedData}),  "SampleFSummarizedData")
      
    }
  )
}