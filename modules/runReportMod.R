runReport_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("reportBuilderButton"), "Run Report", style = "height: 38px;", icon = icon("chart-simple")),
    hr()
  )
}

runReport_Server <- function(id, data, rsdLimits) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns

# modal to appear on button click -----------------------------------------

      
      observeEvent(input$reportBuilderButton, {
        
        showModal(modalDialog(
          title = "Select figures to include in report",
          # SUMMARY TABLE
          checkboxInput(ns("combinedSummariesCheckbox"), "Combined Summaries Tables"),
            conditionalPanel(
              condition = "input.combinedSummariesCheckbox == true",
              #need to tell it to look for namespacing since we're in the server
              ns = ns,
              div(
                style = "margin-left: 25px;", # Indent to the right  margin-top: 10px;
                tags$style(HTML(paste0( #using namespacing below ensures this will only be applied to that element
                  "#", ns("combinedSummariesGroupingOptions"), " .control-label { font-weight: normal; }"
                ))), # Makes the title not bold
                checkboxGroupInput(
                  ns("combinedSummariesGroupingOptions"), 
                  label = "Group By:",
                  choiceNames = c("Species", "Water Name", "Station Code", "Survey ID", "Year"),
                  #values except Year need to match column names 
                  #year column is made in the markdwon before grouping
                  choiceValues = c("CommonName", "WaterName", "StationCode", "SurveyID", "Year")
                )
              )
            ),
          #LENGTH WEIGHT GRAPH
          checkboxInput(ns("lengthWeightCheckbox"), "Length/Weight Graph"),
            conditionalPanel(
              condition = "input.lengthWeightCheckbox == true",
              ns = ns,
              div(
                style = "margin-left: 25px;", # Indent to the right  margin-top: 10px;
                lengthWeightInputs_UI(ns("lengthWeightInputsMod_Report"), class = "normal-label-row"),
              )
            ),
          #LENGTH FREUQNCY GRAPH
          checkboxInput(ns("lengthFrequencyCheckbox"), "Length/Frequency Graph"),
          #lengthFrequency_LengthOptions
          conditionalPanel(
            condition = "input.lengthFrequencyCheckbox == true",
            ns = ns,
            div(
              style = "margin-left: 25px;",
              lengthFrequencyInputs_UI(ns("lengthFrequencyInputsMod_Report"), class = "normal-label-row"),  
            )
          ),
          
          #RELATIVE WEIGHT GRAPH
          checkboxInput(ns("relativeWeightCheckbox"), "Relative Weight Graph"),
          conditionalPanel(
            condition = "input.relativeWeightCheckbox == true",
            ns = ns,
            div(
              style = "margin-left: 25px;",
              relWeightInputs_UI(ns("relWeightInputsMod_Report"), class = "normal-label-row")
            )
          ),
          
          fluidRow(
            column(
              width = 12,
              align = "center", 
              useShinyjs(),
              shinyjs::disabled(
                downloadButton(ns("exportReportButton"), "Export and Save Report", icon = icon("save"))
              )
            )
          ),
          
          footer = tagList(
            fluidRow(
              column(
                width = 12,
                align = "center", 
                modalButton("Cancel")
              )
            )
          ),
          
          easyClose = TRUE, 
          size = "s"
          
        ))
        
      }, ignoreInit = TRUE)
      
      # module reactive inputs return
      lengthWeightsInputs <- lengthWeightInputs_Server("lengthWeightInputsMod_Report")
      lengthFrequencyInputs <- lengthFrequencyInputs_Server("lengthFrequencyInputsMod_Report")
      relWeightsInputs <- relWeightInputs_Server("relWeightInputsMod_Report")
      
      
      observe({
        # Enable only if at least one checkbox is selected
        #for the binwidth one, make sure that frequcny graph is checked (truthy) and input is true. It's true if the conditional binwidth panel doesn't display
        validReportInputs <- isTruthy(input$combinedSummariesCheckbox) || isTruthy(input$lengthWeightCheckbox) || isTruthy(input$lengthFrequencyCheckbox) || isTruthy(input$relativeWeightCheckbox)  #&& lengthFrequencyInputs$validBinWidth())
        if (validReportInputs) {
          shinyjs::enable("exportReportButton")
        } else {
          shinyjs::disable("exportReportButton")
        }
      }, priority = -1)

# rendering markdwon and save logic ---------------------------------------
      output$exportReportButton <- downloadHandler(
        
        filename = function() {
          paste0("SampleFDataReport_", Sys.Date(),  ".html")
          
        },
        
        content = function(file) {
          
          #Create a temporary path for the template
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy("./markdownTemplate/sampleFDataReport.Rmd", tempReport, overwrite = TRUE)
          #need to copy over image as well bc with download handler the data is rendered with a temp directory. 
          #so now the report can "See" the image bc it's also in a temp directory with the markdown
          file.copy("www/CPWLogoLarge.png", file.path(tempdir(), "CPWLogoLarge.png"))
          
          removeModal()
          
          reportParams <- list(
            # get working directoy as a parameter to be able to source files
            appRoot = getwd(),
            sampleFData = data,
            rsdLimits = rsdLimits,
            combinedSummaries = list(
              "display" = isolate(input$combinedSummariesCheckbox), 
              "groupingCols" = isolate(input$combinedSummariesGroupingOptions)
            ),
            lengthWeightGraph = list(
              "display" = isolate(input$lengthWeightCheckbox),
              "lengthOptions" = isolate(lengthWeightsInputs$lengthWeight_LengthOptions), 
              "weightOptions" = isolate(lengthWeightsInputs$lengthWeight_WeightOptions)
            ),
            lengthFrequencyGraph = list(
              "display" = isolate(input$lengthFrequencyCheckbox), 
              "lengthOptions" = isolate(lengthFrequencyInputs$lengthFrequency_LengthOptions), 
              "binwidth" = isolate(lengthFrequencyInputs$lengthFrequencyBinwidthOptions)
              ), 
            relativeWeightGraph = list(
              "display" = isolate(input$relativeWeightCheckbox), 
              "lengthOptions" = isolate(relWeightsInputs$relativeWeight_LengthOptions)
            )
          )
          
          id <- showNotification(
            "Rendering report...",
            duration = NULL,
            closeButton = FALSE
          )
          on.exit(removeNotification(id), add = TRUE)
          
          rmarkdown::render(tempReport, output_file = file,
                            params = reportParams,
                            #envir passes the apps functions/variables
                            envir = new.env(parent = globalenv()))
          
        }
      )
    }
  )
}