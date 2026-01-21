runReport_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("reportBuilderButton"), "Run Report", style = "height: 38px;", icon = icon("chart-simple")),
    hr()
  )
}

runReport_Server <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      values <- reactiveValues()

# modal to appear on button click -----------------------------------------

      
      observeEvent(input$reportBuilderButton, {
        
        showModal(modalDialog(
          title = "Select Figures to Include in Exported Report",
          
          checkboxInput(ns("lengthWeightCheckbox"), "Length/Weight Graph"),
          checkboxInput(ns("lengthFrequencyCheckbox"), "Length/Frequency Graph"),
          
          fluidRow(
            column(
              width = 12,
              align = "center", 
              useShinyjs(),
              downloadButton(ns("exportReportButton"), "Export and Save Report", icon = icon("save"))
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
      
      observe({
        # Enable only if at least one checkbox is selected
        validReportInputs <- isTruthy(input$lengthWeightCheckbox) || isTruthy(input$lengthFrequencyCheckbox)
        
        if (validReportInputs) {
          shinyjs::enable("exportReportButton")
        } else {
          shinyjs::disable("exportReportButton")
        }
      })

# rendering markdwon and save logic ---------------------------------------
      output$exportReportButton <- downloadHandler(
        
        filename = function() {
          paste0("SampleFDataReport_", Sys.Date(),  ".html")
          
        },
        
        content = function(file) {
          
          #Create a temporary path for the template
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy("./markdownTemplate/sampleFDataReport.Rmd", tempReport, overwrite = TRUE)
          
          removeModal()
          
          #showModal(RMDGenerationModal())
          reportParams <- list(
            sampleFData = data,
            lengthWeightGraph = isolate(input$lengthWeightCheckbox),
            lengthFrequencyGraph = isolate(input$lengthFrequencyCheckbox)
          )
          rmarkdown::render(tempReport, output_file = file,
                            params = reportParams,
                            envir = new.env(parent = globalenv()))
          #showNotification("Report successfully saved.")
          #removeModal()
          
        }#,
        #contentType = "application/zip"
      )

      
      
    }
  )
}