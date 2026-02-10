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
      
      #make new shinyvalidate input validator
      iv <- InputValidator$new()
      # add rules for numeric input
      iv$add_rule("lengthFrequencyBinwidthOptions", sv_required()) # Ensure it's not empty
      iv$add_rule("lengthFrequencyBinwidthOptions", sv_numeric())  # Ensure it's a number
      
      # Rule to prevent zero: must be greater than 0
      iv$add_rule("lengthFrequencyBinwidthOptions", sv_gt(0, message = "Value must be greater than 0"))
      #enable validator
      iv$enable()
      
      observeEvent(input$lengthFrequency_LengthOptions, {
        req(iv$is_valid())
        #for graph rendering stuff; tells shiny not to let anything else use binwidth value until it finishes updating
        # prevents "double render/flicker when graph renders in app. Shouldn't make a dif in report rendering ui
        freezeReactiveValue(input, "lengthFrequencyBinwidthOptions")
        
        numericInputVal <- if (input$lengthFrequency_LengthOptions == "Length_inch") 1 else 10
        updateNumericInput(session, "lengthFrequencyBinwidthOptions", value = numericInputVal)
      })
      
      #inputs to return to parent module
      return(
        list(
          "lengthFrequency_LengthOptions" = reactive({ input$lengthFrequency_LengthOptions }),
          "lengthFrequencyBinwidthOptions" = reactive({ input$lengthFrequencyBinwidthOptions }), 
          "binwidthis_valid" = reactive({ iv$is_valid() })
        )
      )
    }
  )
}