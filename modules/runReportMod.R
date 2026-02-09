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
      
      #make new shinyvalidate input validator
      iv <- InputValidator$new()
      # add rules for numeric input
      iv$add_rule("lengthFrequencyBinwidthOptions", sv_required()) # Ensure it's not empty
      iv$add_rule("lengthFrequencyBinwidthOptions", sv_numeric())  # Ensure it's a number
      
      # Rule to prevent zero: must be greater than 0
      iv$add_rule("lengthFrequencyBinwidthOptions", sv_gt(0, message = "Value must be greater than 0"))
      #enable validator
      iv$enable()
      
      #values <- reactiveValues()

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
                tags$style(HTML(paste0( #using namespacing below ensures this will only be applied to that element
                  "#", ns("lengthWeight_LengthOptions"), " .control-label, ", 
                  "#", ns("lengthWeight_WeightOptions"), " .control-label ",
                  "{ font-weight: normal; }"
                ))), # Makes the title not bold
                fluidRow(
                  column(6, 
                         radioButtons(
                           ns("lengthWeight_LengthOptions"), 
                           label = "Length Display",
                           choiceNames = c("Millimeters", "Inches"),
                           #values need to match column names 
                           choiceValues = c("Length_mm", "Length_inch")
                         )
                         ), 
                  column(6, 
                         radioButtons(
                           ns("lengthWeight_WeightOptions"), 
                           label = "Weight Display",
                           choiceNames = c("Grams", "Ounces"),
                           #values need to match column names 
                           choiceValues = c("Weight_g", "Weight_oz")
                         )
                         )
                )
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
              # tags$style(HTML(paste0( #using namespacing below ensures this will only be applied to that element
              #   "#", ns("lengthFrequency_LengthOptions"), " .control-label, ", 
              #   "#", ns("lengthFrequencyBinwidthOptions"), "-label ",
              #   "{ font-weight: normal !important; }"
              # ))), # Makes the title not bold
              lengthFrequencyInputs_UI(ns("lengthFrequencyInputsMod_Report"), class = "normal-label-row"),
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
      
      # update binwidth based on button click; default 1 inch or 10 mm
      observeEvent(input$lengthFrequency_LengthOptions, {
        numericInputVal <- if (input$lengthFrequency_LengthOptions == "Length_inch") 1 else 10
        updateNumericInput(session, "lengthFrequencyBinwidthOptions", value = numericInputVal)
      })
      
      observe({
        # Enable only if at least one checkbox is selected
        #for the binwidth one, make sure that frequcny graph is checked (truthy) and input is true. It's true if the conditional binwidth panel doesn't display
        validReportInputs <- isTruthy(input$combinedSummariesCheckbox) || isTruthy(input$lengthWeightCheckbox) || isTruthy(input$lengthFrequencyCheckbox) # && lengthFrequencyInputs$validBinWidth()
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
              "lengthOptions" = isolate(input$lengthWeight_LengthOptions), 
              "weightOptions" = isolate(input$lengthWeight_WeightOptions)
            ),
            lengthFrequencyGraph = list(
              "display" = isolate(input$lengthFrequencyCheckbox), 
              "lengthOptions" = isolate(input$lengthFrequency_LengthOptions), 
              "binwidth" = isolate(input$lengthFrequencyBinwidthOptions)
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