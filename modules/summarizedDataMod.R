# sql2<-"Select * from CurrentSummary where year(SampleDate)=2025"
# SummarizedData<- dbGetQuery(CPW_AqDatAnalysis, sql2)
data <- tbl(CPW_AqDatAnalysis, "CurrentSummary")
allYears <- data %>%
  distinct(year(SampleDate)) %>%
  pull()

allBios <- data %>%
  distinct(AreaBio) %>%
  show_query() %>%
  pull() 

# allSPBios <- data %>%
#   distinct(SpConBio) %>%
#   show_query() %>%
#   pull() 

# allStationCodes <- data %>%
#   distinct(StationCode) %>%
#   show_query() %>%
#   pull() 

summarizedData_UI <- function(id) {
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
        virtualSelectInput(ns("waterNameSearch"),
                           label = "Water Name",
                           choices = allDistinctWaters,
                           multiple = TRUE,
                           search = TRUE,          
                           autoSelectFirstOption = FALSE, 
                           #this ensures the dropdown is fully visible over the slider
                           dropboxWrapper = "body" 
        ), 
        sliderInput(ns("yearSlider"), "Date",
                    min = min(allYears, na.rm = TRUE),
                    max = max(allYears, na.rm = TRUE),  
                    value = c(min(allYears, na.rm = TRUE), max(allYears, na.rm = TRUE)),
                    step = 1, 
                    sep = ""
        ),

        
        # uiOutput(ns("yearsSearchUI")),


        actionButton(ns("queryButton"),
                     label = "Render Data", width = "100%")

      ),

      mainPanel(
        uiOutput(ns("mainPanelUI"))
      )
      # mainPanel(
      #   DTOutput(ns("currentSummaryData")),
      #   downloadData_UI(ns("downloadCurentSummaryData"))
      # )
    )



  )
}

summarizedData_Server <- function(id, tableName) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      

# UI Components -----------------------------------------------------------

      output$mainPanelUI <- renderUI({
        #only run this block when this button is clicked
        input$queryButton
        #do NOT re-run this block just becuase the values changed; wait for input$queryButton
        yearInputCheck <- isolate(isTruthy(input$yearSlider))
        #if button hasn't been clicked at all yet, retun this message
        if (input$queryButton == 0) {
          return(p("Please select a Area Bio, Water Name, or year range and click 'Render'.", 
                   style = "color: gray;"))
        }
        #check if waterNames or Station Code inputs are valid, and return a message if not
        if (!yearInputCheck) {
          return(p("Please select a valid year range before rendering.", 
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

# Data Wrangling ----------------------------------------------------------

      
      currentSummaryDataToDisplay <- eventReactive(input$queryButton,ignoreNULL = TRUE,{
        yearMin <- as.integer(input$yearSlider[1])
        yearMax <- as.integer(input$yearSlider[2])
        waterNames <- input$waterNameSearch
        areaBios <- input$areaBioSearch
        
        data <- tbl(CPW_AqDatAnalysis, tableName) %>%
          filter(year(SampleDate) >= yearMin & year(SampleDate) <= yearMax) %>%
          show_query()
        
        # if(isTruthy(waterNames)){
        #   data <- data %>%
        #     filter(WaterName %in% waterNames
        #     ) 
        # }
        # 
        # if(isTruthy(areaBios)){
        #   data <- data %>%
        #     #!! bang bang operator tells it to evaluate this statement instead of looking for a column named areaBios; not sure if 100% needed but ok
        #     filter(AreaBio %in% !!areaBios)
        #   
        # }
        print("about to collect")
        finalFilteredData <- data %>%
          show_query() %>%
          collect()
        #columns in this db are "blobs" type which are found in DBs I guess. this converts them to character type and allows DT to display them
        finalFilteredData1 <- finalFilteredData %>%
          mutate(across(where(~inherits(., "blob")), 
                        ~sapply(., function(x) paste(as.character(x), collapse = ""))))
        
        return(finalFilteredData1)
      })
      
      output$currentSummaryData <- renderDT(server = TRUE, {
        
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