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

allStationCodes <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
  distinct(StationCode) %>%
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
        virtualSelectInput(ns("stationCodeSearch"),
                           label = "Station Code",
                           choices = sort(allStationCodes),
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
        # uiOutput(ns("downloadDataUI")),
        # box(
        #   withSpinner(DTOutput(ns("sampleFData")))
        # )
        
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
      
      # waterNames <- reactiveVal("")
      # waterNames <- input$waterNameSearch
      

# UI Components -----------------------------------------------------------
      
      output$mainPanelUI <- renderUI({
        input$queryButton
        waterNameInputCheck <- isolate(isTruthy(input$waterNameSearch))
        stationCodeInputCheck <- isolate(isTruthy(input$stationCodeSearch))
        
        if (input$queryButton == 0) {
          return(p("Please select Please select a Area Bio, Species Con Bio, Water Name, or Station Code and click 'Render'.", 
                   style = "color: gray;"))
        }
        if (!(waterNameInputCheck || stationCodeInputCheck)) {
          return(p("Please select a Water Name or Station Code before rendering.", 
                   style = "color: red;"))
        }
        # validate(
        #   need(isTruthy(input$waterNameSearch) || isTruthy(input$stationCodeSearch), "Please select a Area Bio, Species Con Bio, Water Name, or Station Code and click 'Render'")
        # )
        tagList(
          uiOutput(ns("downloadDataUI")),
          box(
            withSpinner(DTOutput(ns("sampleFData")))
          )
        )
      })
      # #save data option only appears if there's a valid dataset to download
      output$downloadDataUI <- renderUI({
        #print(isTruthy(sampleFDataToDisplay()))
        # validate(
        #   need(nrow(sampleFDataToDisplay()) > 0, "Please select a Water Name or Station Code")
        # )
        req(nrow(sampleFDataToDisplay()) > 0)
        print("downloadUI rendered")
        downloadData_UI(ns("downloadSampleFData"))
      })
      
      #slider renders and updates with changes to each of the waterNames or station codes
      #waternames changes based on sp con bio or area bio
      
      output$yearSliderUI <- renderUI({
        #print("slider render")
        #\|| means that second element will be only be evaluated if first isn't true; not sure if it matters here but probably speeds it up a tad
        if(isTruthy(input$waterNameSearch) || isTruthy(input$stationCodeSearch)) { #|| isTruthy(input$SpConBioSearch) #|| isTruthy(input$areaBioSearch) 
          #update years based on waterName,
          waterNames <- input$waterNameSearch
          stationCodes <- input$stationCodeSearch
          
          data <- tbl(CPW_AqDatAnalysis, "SampleFView")
          
          if(isTruthy(waterNames)){
            data <- data %>%
              filter(WaterName %in% waterNames)
          }
          
          if(isTruthy(stationCodes)){
            data <- data %>%
              filter(StationCode %in% stationCodes)
          }
          
          allyears <- data %>%
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
                        #timeFormat = "%y"
            )
          )
          
        }
      })
      # if any of these updates, I want the waterName element to update
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
          freezeReactiveValue(input, "waterNameSearch")
          freezeReactiveValue(input, "stationCodeSearch")
          
          #update waterNmaes based on bio selection
          selectedWaterNames <- table %>%
            distinct(WaterName) %>%
            #show_query() %>%
            #collect() is when the query actually runs, just builds a query until then
            #returns as df
            collect() %>%
            #just pulls out the one column
            pull() %>%
            as.character()
          #unname()
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
            as.character()
         

          updateVirtualSelect(
            session = session,
            "stationCodeSearch", 
            choices = sort(selectedStationCodes), 
            selected = selectedStationCodes
          ) 

        } else {
          print("waternams uptadad to all waternames")
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
        
        
        
        # if(isTruthy(waterNames)){
        #   selectedWatersOnly <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
        #     filter(WaterName %in% waterNames)
        #   selectedBios <- selectedWatersOnly %>%
        #     distinct(AreaBio) %>%
        #     collect() %>%
        #     pull() 
        #   #unname()
        #   #error: in as.vector: cannot coerce type 'environment' to vector of type 'character' solved by explicitly making it a character. 
        #   cleanChoices <- as.character(selectedBios)
        #   
        #   #print(cleanChoices)
        #   updateVirtualSelect(
        #     session = session,
        #     "areaBioSearch", 
        #     choices = cleanChoices, 
        #     selected = cleanChoices
        #     
        #   )
        #   
        # }
        
        
        
        
        # selectedWatersOnly <- tbl(CPW_AqDatAnalysis, "SampleFView") %>%
        #   filter(WaterName %in% waterNames)
        # allyears <- selectedWatersOnly %>%
        #   distinct(year(SampleDate)) %>%
        #   pull()

        # updateSliderInput(
        #   session,
        #   "yearSlider",
        #   min = min(allyears),
        #   max = max(allyears),
        #   value = c(min(allyears), max(allyears))
        # )

      }, ignoreInit = TRUE)
      

# data wrangling ----------------------------------------------------------
      
      sampleFDataToDisplay <- eventReactive(input$queryButton, ignoreNULL = TRUE, {
        #double fires
        # validate(
        #   need(isTruthy(input$waterNameSearch) || isTruthy(input$stationCodeSearch), "Please select a Water Name or Station Code")
        # )
        
        req(isTruthy(input$waterNameSearch) || isTruthy(input$stationCodeSearch))
        print("query run")
        

        ##ERROR: Error in .transformer: `value` must be a string or scalar SQL, not the number 1. 
        #caused because it's hard to dbplyr to translate R to sql with lists directly inside a filter for a remote database table
        #making variables beforehand alllows us to to use them as "scalars" that it knows how to converrt to SQL values
        yearMin <- as.integer(input$yearSlider[1])
        yearMax <- as.integer(input$yearSlider[2])
        waterNames <- input$waterNameSearch
        stationCodes <- input$stationCodeSearch
        
        areaBios <- input$areaBioSearch
        spConBios <- input$SpConBioSearch
        
        data <- tbl(CPW_AqDatAnalysis, tableName) %>%
          filter(year(SampleDate) >= yearMin & year(SampleDate) <= yearMax)
        if(isTruthy(waterNames)){
          data <- data %>%
            filter(WaterName %in% waterNames
            ) 
        }
        if(isTruthy(stationCodes)){
          data <- data %>%
            filter(StationCode %in% stationCodes
            ) 
        }
          
        if(isTruthy(areaBios)){
          data <- data %>%
            #!! bang bang operator tells it to evaluate this statement instead of looking for a column named areaBios; not sure if 100% needed but ok
            filter(AreaBio %in% !!areaBios)
          
        }#allows it so query builds like a AND statement
        if(isTruthy(spConBios)) {
          data <- data %>%
            filter(SpConBio %in% !!spConBios)
        } 
        
        finalFilteredData <- data %>%
          #show_query() %>%
          collect()
        
        return(finalFilteredData)
        
      })

# Output display ----------------------------------------------------------
      
      
      output$sampleFData <- renderDT({
        #input$queryButton
        req(sampleFDataToDisplay())
        #print(sampleFDataToDisplay())
        # validate(
        #   need(isTruthy(sampleFDataToDisplay()), "Please select a Water Name or Station Code")
        # )
        # validate(
        #   need(isTruthy(input$waterNameSearch) || isTruthy(input$stationCodeSearch), "Please select a Water Name or Station Code")
        # )
        print("datatablke render")
        
        datatable(sampleFDataToDisplay(),
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
        #server = FALSE
      }, server = TRUE)
      
      #not using sampleFDataToDisplay() because that unwraps the object and passes the static result of the data at that exact moment. instead, 
      #sampleFDataToDisplay passes the reactive object itself and tells the mod to "go get" the data
      #same idea around making the filename reactive. one option is reactive({ paste0(input$waterNameSearch) })
      
      downloadData_Server("downloadSampleFData", sampleFDataToDisplay,  "SampleFData")
      
      
      
    }
  )
}