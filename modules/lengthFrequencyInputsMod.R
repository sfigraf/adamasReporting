# lengthFrequencyGraphInputs <- function(ns, idPrefix = "", class = "green-row"){
#   fluidRow(class = class, 
#            column(6,
#                   radioButtons(
#                     ns("lengthFrequency_LengthOptions"), 
#                     label = "Length Display",
#                     choiceNames = c("Millimeters", "Inches", "RSD Counts"),
#                     #values need to match column names 
#                     choiceValues = c("Length_mm", "Length_inch", "RSD")
#                   )
#            ),
#            column(6,
#                   conditionalPanel(
#                     condition = "input['lengthFrequency_LengthOptions'] != 'RSD'",
#                     ns = ns,
#                     numericInput(ns("lengthFrequencyBinwidthOptions"), "Binwidth", value = 10, 
#                                  min = 0)
#                   )
#            )
#   )
# }

lengthFrequencyInputs_UI <- function(id, class = "green-row") {
  ns <- NS(id)
  tagList(
    fluidRow(class = class, 
             column(6,
                    radioButtons(
                      ns("lengthFrequency_LengthOptions"), 
                      label = "Length Display",
                      choiceNames = c("Millimeters", "Inches", "RSD Counts"),
                      #values need to match column names 
                      choiceValues = c("Length_mm", "Length_inch", "RSD")
                    )
             ),
             column(6,
                    conditionalPanel(
                      condition = "input['lengthFrequency_LengthOptions'] != 'RSD'",
                      ns = ns,
                      numericInput(ns("lengthFrequencyBinwidthOptions"), "Binwidth", value = 10, 
                                   min = 0)
                    )
             )
    )
  )
}

lengthFrequencyInputs_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$lengthFrequency_LengthOptions, {
        numericInputVal <- if (input$lengthFrequency_LengthOptions == "Length_inch") 1 else 10
        updateNumericInput(session, "lengthFrequencyBinwidthOptions", value = numericInputVal)
      })
    }
  )
}