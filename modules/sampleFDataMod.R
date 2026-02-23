sampleFData_UI <- function(id, initialValues) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
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
                     label = "Render Data", width = "100%", class = "btn-cpw-sidebar"), 
        
        h6("Note: entries with NA values in any of the filter fields are excluded from the results")
        
      ),
      
      mainPanel(
        uiOutput(ns("mainPanelUI"))
      )
    )
  
  )
}

sampleFData_Server <- function(id, sampleFDataAsTable, initialValues, rsdLimits) {
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
        
        #if there is anything selected in either areaBios or Specis bios, udpate waternames and station code options
        #else, just go back to default options
        if(isTruthy(areaBios) || isTruthy(spConBios)){
          if(isTruthy(areaBios)){
            sampleFDataAsTable <- sampleFDataAsTable %>%
              #!! bang bang operator tells it to evaluate this statement instead of looking for a column named areaBios; not sure if 100% needed but ok
              filter(AreaBio %in% !!areaBios)
            
          }#allows it so query builds like a AND statement
          if(isTruthy(spConBios)) {
            sampleFDataAsTable <- sampleFDataAsTable %>%
              filter(SpConBio %in% !!spConBios)
          } 
          
          ##freeze these input reactive values and ignore downstream observers (in this case, the Slider UI render) until they have completely finisheed
          #this prevents slider from "re-rendering" once first for watername update and again for stationcode update
          #could also try looking into debounce() to wait a few milliseconds for the reactives to settle
          #from shiny website:  it’s actually good practice to always use it when you dynamically change an input value. The actual modification takes some time to flow to the browser then back to Shiny, and in the interim any reads of the value are at best wasted, and at worst lead to errors. Use freezeReactiveValue() to tell all downstream calculations that an input value is stale and they should save their effort until it’s useful.
          #https://mastering-shiny.org/action-dynamic.html
          freezeReactiveValue(input, "waterNameSearch")
          freezeReactiveValue(input, "stationCodeSearch")
          freezeReactiveValue(input, "surveyIDSearch")
          freezeReactiveValue(input, "lengthSlider")
          
          
          #update waterNmaes based on bio selection
          selectedWaterNames <- sampleFDataAsTable %>%
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
          selectedStationCodes <- sampleFDataAsTable %>%
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
          
          #update SurveyID based on bio selection
          selectedSurveyIDs <- sampleFDataAsTable %>%
            distinct(SurveyID) %>%
            collect() %>%
            pull() %>%
            as.numeric() %>%
            sort() 
          
          updateVirtualSelect(
            session = session,
            "surveyIDSearch", 
            choices = sort(selectedSurveyIDs), 
            selected = selectedSurveyIDs
          ) 
          
        } else {
          updateVirtualSelect(
            session = session,
            "waterNameSearch", 
            choices = initialValues$allDistinctWaters
          )
          
          updateVirtualSelect(
            session = session,
            "stationCodeSearch", 
            choices = initialValues$allStationCodes
          )
          
          updateVirtualSelect(
            session = session,
            "surveyIDSearch", 
            choices = initialValues$allSurveyIDs
          )
          
        }
        
      }, ignoreInit = TRUE) #not sure why i don't need ignoreNull here and it works but whatever
      
      #year slider renders and updates with changes to each of the waterNames or station codes
      #waternames an changes based on sp con bio or area bio above
      
      output$yearSliderUI <- renderUI({
        #\|| means that second element will be only be evaluated if first isn't true; not sure if it matters here but probably speeds it up a tad
        if(isTruthy(input$waterNameSearch) || isTruthy(input$stationCodeSearch) || isTruthy(input$surveyIDSearch)) { #|| isTruthy(input$SpConBioSearch) #|| isTruthy(input$areaBioSearch)
          #update years based on waterName,
          waterNames <- input$waterNameSearch
          stationCodes <- input$stationCodeSearch
          surveyIDs <- input$surveyIDSearch
          
          #redundant but helps me keep straight in my head
          sampleFForSlider <- sampleFDataAsTable

          if(isTruthy(waterNames)){
            sampleFForSlider <- sampleFForSlider %>%
              filter(WaterName %in% waterNames)
          }

          if(isTruthy(stationCodes)){
            sampleFForSlider <- sampleFForSlider %>%
              filter(StationCode %in% stationCodes)
          }
          
          if(isTruthy(surveyIDs)){
            sampleFForSlider <- sampleFForSlider %>%
              filter(SurveyID %in% surveyIDs)
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
        
        if(isTruthy(input$waterNameSearch) || isTruthy(input$stationCodeSearch) || isTruthy(input$surveyIDSearch)) { #|| isTruthy(input$SpConBioSearch) #|| isTruthy(input$areaBioSearch) 
          
          yearMin <- as.integer(input$yearSlider[1])
          yearMax <- as.integer(input$yearSlider[2])
          
          #update years based on waterName,
          waterNames <- input$waterNameSearch
          stationCodes <- input$stationCodeSearch
          surveyIDs <- input$surveyIDSearch
          
          sampleFForLengthSlider <- sampleFDataAsTable
          
          if(isTruthy(waterNames)){
            sampleFForLengthSlider <- sampleFForLengthSlider %>%
              filter(WaterName %in% waterNames)
          }
          
          if(isTruthy(stationCodes)){
            sampleFForLengthSlider <- sampleFForLengthSlider %>%
              filter(StationCode %in% stationCodes)
          }
          
          if(isTruthy(surveyIDs)){
            sampleFForLengthSlider <- sampleFForLengthSlider %>%
              filter(SurveyID %in% surveyIDs)
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
        surveyIDInputCheck <- isolate(isTruthy(input$surveyIDSearch))
        lengthInputCheck <- isolate(all(is.numeric(input$lengthSlider)))
        
        #tracks which tab within each tabset is selected and ui renders to that tab after new data render
        currentSampleFTabsTabset <- isolate(input$sampleFTabsTabset) %||% "tablesTab"
        currentSampleFTableTabsTab <- isolate(input$sampleFTableTabsTabset) %||% "rawDataTab"
        currentSampleFGraphsTab <- isolate(input$sampleFGraphsTabset) %||% "lengthWeightsGraphsTab"

        #if button hasn't been clicked at all yet, retun this message
        if (input$queryButton == 0) {
          return(p("Please select a Area Bio, Species Con Bio, Water Name, or Station Code and click 'Render'.", 
                   style = "color: gray;"))
        }
        #check if waterNames or Station Code or survey ID inputs are valid, and return a message if not
        if (!(waterNameInputCheck || stationCodeInputCheck || surveyIDInputCheck)) {
          return(p("Please select a Water Name, Station Code, or Survey ID before rendering.", 
                   style = "color: gray;"))
        }
        #pretty much every time an input is called explicitly it should be wrapped in isolate() within this block to prevent UI render before input$querybutton is clicked
        if (isolate(input$lengthFilter) & !(lengthInputCheck)) {
          return(p("No data collected for selected year(s) or only NA lengths detected at this water. Please turn off length filter before rendering this data.",
                   style = "color: gray;"))
        }
        #if we make it this far, it's because all the previous conditions are met and we can successfully render the UI
        tagList(
          tabsetPanel(id = ns("sampleFTabsTabset"),
                      selected = currentSampleFTabsTabset,
            tabPanel("Tables", value = "tablesTab",
                     tabsetPanel(id = ns("sampleFTableTabsTabset"),
                                 selected = currentSampleFTableTabsTab,
                       tabPanel("Raw Data", value = "rawDataTab", 
                                div(style = "display: flex; gap: 10px; margin-bottom: 10px; margin-top: 10px;",
                                    uiOutput(ns("downloadDataUI")),
                                    uiOutput(ns("reportBuilderUI"))
                                ),
                                box(
                                  withSpinner(DTOutput(ns("sampleFData")))
                                )
                       ), 
                       tabPanel("Combined Summaries", value = "combinedSummariesTab", 
                                wellPanel(
                                  fluidRow(
                                    column(12, 
                                           pickerInput(ns("combinedSummariesGroupingOptions"), "Group By",
                                                       #should be the same options as what we have in render report
                                                       choices = c("Species" = "CommonName", "Water Name" = "WaterName",
                                                                   "Station Code" = "StationCode", "Survey ID" = "SurveyID", "Year" = "Year"),
                                                       multiple = TRUE, 
                                                       selected = "CommonName"
                                                       #values except Year need to match column names 
                                                       #year column is made in the markdwon before grouping
                                                       
                                                       )
                                           
                                           )
                                  ),
                                #br(),
                                h3(paste0("Mean, Min, Max Length and Weight")), #by ", isolate(input$combinedSummariesGroupingOptions))),
                                  withSpinner(DTOutput(ns("sampleFSummarizedMeanTable"))),
                                h3(paste0("Proportional Stocking Density and Catch/Unit Effort")),# by ", isolate(input$combinedSummariesGroupingOptions))),
                                br(),
                                  withSpinner(DTOutput(ns("sampleFSummarizedStockDensity"))),
                                h3(paste0("Relative Abundance and Catch Per Unit Effort")), #by ", isolate(input$combinedSummariesGroupingOptions))),
                                  withSpinner(DTOutput(ns("sampleFSummarizedCPUE")))
                                )
                                
                                
                       )
                     )
                     
            ), 
            tabPanel("Graphs", value = "sampleFGraphsTab", 
                     tabsetPanel(id = ns("sampleFGraphsTabset"), 
                                 selected = currentSampleFGraphsTab,
                       tabPanel("Length/Weights", value = "lengthWeightsGraphsTab", 
                                wellPanel(
                                  lengthWeightInputs_UI(ns("lengthWeightInputsMod")),
                                  withSpinner(plotlyOutput(ns("lengthWeightsGraph")))
                                )
                                
                       ), 
                       tabPanel("Length Frequencies", value = "lengthFreqsGraphsTab", 
                                wellPanel(
                                  lengthFrequencyInputs_UI(ns("lengthFrequencyInputsMod")),
                                  withSpinner(plotlyOutput(ns("lengthFrequenciesGraph")))
                                )
                                
                       ), 
                       tabPanel("Relative Weight", value = "relWeightsGraphsTab", 
                                wellPanel(
                                  relWeightInputs_UI(ns("relWeightInputsMod")),
                                  withSpinner(plotlyOutput(ns("relativeWeightGraph")))
                                )
                                
                       )
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
      
      # module reactive inputs return
      lengthWeightsInputs <- lengthWeightInputs_Server("lengthWeightInputsMod")
      lengthFrequencyInputs <- lengthFrequencyInputs_Server("lengthFrequencyInputsMod")
      relWeightsInputs <- relWeightInputs_Server("relWeightInputsMod")


# data wrangling ----------------------------------------------------------
      
      sampleFDataList <- eventReactive(input$queryButton, ignoreNULL = TRUE, {
        
        #only run if one of these are true. if not, it will get get caught in the render UI above
        req(isTruthy(input$waterNameSearch) || isTruthy(input$stationCodeSearch) || isTruthy(input$surveyIDSearch))

        ##ERROR: Error in .transformer: `value` must be a string or scalar SQL, not the number 1. 
        #caused because it's hard to dbplyr to translate R to sql with lists directly inside a filter for a remote database table
        #making variables beforehand alllows us to to use them as "scalars" that it knows how to converrt to SQL values
        yearMin <- as.integer(input$yearSlider[1])
        yearMax <- as.integer(input$yearSlider[2])
        waterNames <- input$waterNameSearch
        stationCodes <- input$stationCodeSearch
        surveyIDs <- input$surveyIDSearch
        areaBios <- input$areaBioSearch
        spConBios <- input$SpConBioSearch
        lengthMin <- as.integer(input$lengthSlider[1])
        lengthMax <- as.integer(input$lengthSlider[2])
        
        #filter data based on inputs
        #allows it so query builds like a AND statement
        samplFDataFiltered <- sampleFDataAsTable %>%
          filter(year(SampleDate) >= yearMin & year(SampleDate) <= yearMax)
        
        if(isTruthy(waterNames)){
          samplFDataFiltered <- samplFDataFiltered %>%
            filter(WaterName %in% waterNames) 
        }
        
        if(isTruthy(stationCodes)){
          samplFDataFiltered <- samplFDataFiltered %>%
            filter(StationCode %in% stationCodes) 
        }
        
        if(isTruthy(surveyIDs)){
          samplFDataFiltered <- samplFDataFiltered %>%
            filter(SurveyID %in% surveyIDs) 
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
  
        finalFilteredDataList <- list(
          "sampleFRawDataToDisplay" = finalFilteredData 
        )
        
        return(finalFilteredDataList)
        
      })
      
      ##get just tables from group buttons
      sampleFCombinedSummarizedData <- eventReactive(list(input$combinedSummariesGroupingOptions, input$queryButton) , {
        req(isTruthy(sampleFDataList()))
        sampleFCombinedSummarizedData <- getCombinedSummariesTables(input$combinedSummariesGroupingOptions, data = sampleFDataList()$sampleFRawDataToDisplay) #%>%
        return(sampleFCombinedSummarizedData)
        
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
      
      output$sampleFSummarizedMeanTable <- renderDT({
        
        req(sampleFCombinedSummarizedData()$meanMinMaxLengthWeightsTable)
        sampleFCombinedSummarizedData()$meanMinMaxLengthWeightsTable
        
      }, server = TRUE)
      
      output$sampleFSummarizedStockDensity <- renderDT({
        
        req(sampleFCombinedSummarizedData()$proportionalstockdensityTable)
        sampleFCombinedSummarizedData()$proportionalstockdensityTable
        
      }, server = TRUE)
      
      output$sampleFSummarizedCPUE <- renderDT({
        
        req(sampleFCombinedSummarizedData()$relAbundanceCPUETable)
        sampleFCombinedSummarizedData()$relAbundanceCPUETable
        
      }, server = TRUE)
      
      output$lengthWeightsGraph <- renderPlotly({
        getLengthWeightGraph(data = sampleFDataList()$sampleFRawDataToDisplay, lengthWeightsInputs$lengthWeight_LengthOptions(), lengthWeightsInputs$lengthWeight_WeightOptions())
      })
      
      output$lengthFrequenciesGraph <- renderPlotly({

        getLengthFrequenciesGraph(data = sampleFDataList()$sampleFRawDataToDisplay, 
                                  lengthOptions = lengthFrequencyInputs$lengthFrequency_LengthOptions(),
                                  binwidth = lengthFrequencyInputs$lengthFrequencyBinwidthOptions(), 
                                  rsdLimits = rsdLimits)
      })
      
      output$relativeWeightGraph <- renderPlotly({
        
        getRelativeWeightGraph(data = sampleFDataList()$sampleFRawDataToDisplay, 
                                  lengthOptions = relWeightsInputs$relativeWeight_LengthOptions()
                                  )
      })
      
      
      
      #not using sampleFDataList()$sampleFRawDataToDisplay because that unwraps the object and passes the static result of the data at that exact moment. instead, 
      #reactive({sampleFDataList()$sampleFRawDataToDisplay}) passes the reactive object itself and tells the mod to "go get" the data
      #same idea around making the filename reactive. one option is reactive({ paste0(input$waterNameSearch) })
      
      #sample f rawe data tab
      downloadData_Server("downloadSampleFData", reactive({sampleFDataList()$sampleFRawDataToDisplay}),  "SampleFData")
      runReport_Server("reportBuilder", reactive({sampleFDataList()$sampleFRawDataToDisplay}), rsdLimits = rsdLimits)
      #summarized data tab
      downloadData_Server("downloadSampleFSummarizedData", reactive({sampleFDataList()$sampleFSummarizedData}),  "SampleFSummarizedData")
      
    }
  )
}