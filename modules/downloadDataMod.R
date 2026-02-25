downloadData_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("downloadActionButton"), label = "Save Data", style = "padding-left: 20px; border: none; background: none; width: 100%; text-align: left; height: 38px;", 
                 icon = icon("download"), class = "nav-link"), 
    hr(),
  )
}

downloadData_Server <- function(id, data, fileName = "ADAMASDataDownload") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$downloadActionButton, {
        
        showModal(modalDialog(
          fluidRow(
            column(
              width = 12,
              align = "center", 
              downloadButton(ns("downloadCSV"), "Download as CSV", class = ".btn-cpw-general")
            )
          ), 
          br(), 
          
          fluidRow(
            column(
              width = 12,
              align = "center", 
              downloadButton(ns("downloadExcel"), "Download as Excel", class = ".btn-cpw-general")
            )
          ), 
          br(), 
          
          fluidRow(
            column(
              width = 12,
              align = "center", 
              downloadButton(ns("downloadRDS"), "Download as RDS", class = ".btn-cpw-general")
            )
          ),
          br(),
          
          footer = tagList(
            fluidRow(
              column(
                width = 12,
                align = "center", 
                tagAppendAttributes(
                  modalButton("Cancel"), 
                  class = "btn-cpw-general"
                )
              )
            )
          ),
          
          easyClose = TRUE, 
          size = "s"
          
        ))
      }, ignoreInit = TRUE)
      
      output$downloadCSV <- downloadHandler(
        filename = function() {
          filenameReactive <- if (shiny::is.reactive(fileName)) fileName() else fileName
          paste(filenameReactive, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          on.exit(removeModal())
          #grabs the current version of that data with this call using ()
          data <- if(is.reactive(data)) data() else data
          
          id <- showNotification(
            "Collecting data to save...",
            duration = NULL,
            closeButton = FALSE
          )
          on.exit(removeNotification(id), add = TRUE)
          write_csv(data, file, progress = TRUE)
          
        }
      )
      
      output$downloadExcel <- downloadHandler(
        filename = function() {
          filenameReactive <- if (shiny::is.reactive(fileName)) fileName() else fileName
          paste(filenameReactive, "_", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
          on.exit(removeModal())
          #grabs the current version of that data with this call using ()
          data <- if(is.reactive(data)) data() else data
          id <- showNotification(
            "Collecting data to save...",
            duration = NULL,
            closeButton = FALSE
          )
          on.exit(removeNotification(id), add = TRUE)
          
          openxlsx::write.xlsx(data, file)

        }
      )
      
      
      output$downloadRDS <- downloadHandler(
        filename = function() {
          filenameReactive <- if (shiny::is.reactive(fileName)) fileName() else fileName
          paste(filenameReactive, "_", Sys.Date(), ".rds", sep = "")
        },
        content = function(file) {
          on.exit(removeModal())
          #grabs the current version of that data with this call using ()
          data <- if(is.reactive(data)) data() else data
          
          id <- showNotification(
            "Collecting data to save...",
            duration = NULL,
            closeButton = FALSE
          )
          on.exit(removeNotification(id), add = TRUE)
          
          saveRDS(data, file = file)
        }
      )
      
    }
  )
}