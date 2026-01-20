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

# rendering markdwon and save logic ---------------------------------------
      output$exportReportButton <- downloadHandler(
        
        filename = function() {
          paste("SampleFDataReport_", Sys.Date(),  ".html")
          
        },
        
        content = function(file) {
          
          #Create a temporary path for the template
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy("./markdownTemplate/sampleFDataReport.Rmd", tempReport, overwrite = TRUE)
          
          
          removeModal()
          
          #showModal(RMDGenerationModal())
          
          rmarkdown::render(tempReport, output_file = file,
                            #params = values$params,
                            envir = new.env(parent = globalenv()))
          showNotification("Report successfully saved.")
          #removeModal()
          
        }#,
        #contentType = "application/zip"
      )

      
      
    }
  )
}