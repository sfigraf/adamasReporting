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
      
      #set reactive value to track when the binwidth input is updating
      isUpdating <- reactiveVal(FALSE)
      #make new shinyvalidate input validator
      iv <- InputValidator$new()
      
      #add condition to only apply validator rules at specifc time to avoid "required" flicker
     # print(input$lengthFrequencyBinwidthOptions)
      # set condition on validator to only apply the rules when the numeric bindith input is not updating

      iv$condition(~ !isUpdating())
      # add rules for numeric input
      iv$add_rule("lengthFrequencyBinwidthOptions", sv_required()) # Ensure it's not empty
      iv$add_rule("lengthFrequencyBinwidthOptions", sv_numeric())  # Ensure it's a number
      
      # Rule to prevent zero: must be greater than 0
      iv$add_rule("lengthFrequencyBinwidthOptions", sv_gt(0, message = "Value must be greater than 0"))
      #enable validator
      iv$enable()
      
      
      
      observeEvent(input$lengthFrequency_LengthOptions, {
        #print(input$lengthFrequencyBinwidthOptions)
        #req(iv$is_valid())
        #iv$disable() 
        # set reactive value to true to disable the validation rules while the binwidth is being updated
        isUpdating(TRUE)
        #for graph rendering stuff; tells shiny not to let anything else use binwidth value until it finishes updating
        # prevents "double render/flicker when graph renders in app. Shouldn't make a dif in report rendering ui
        freezeReactiveValue(input, "lengthFrequencyBinwidthOptions")
        
        numericInputVal <- if (input$lengthFrequency_LengthOptions == "Length_inch") 1 else 10
        updateNumericInput(session, "lengthFrequencyBinwidthOptions", value = numericInputVal)
        
        #after the binwidth value is updated, set the reactive value back to false to re-enable validator rules
        # 
        #session$onFlushed(function() {
          #
          later::later(function() { isUpdating(FALSE) }, 0.1)
        #}, once = TRUE)
        
      })
      # target_binwidth <- eventReactive(input$lengthFrequency_LengthOptions, {
      #   req(input$lengthFrequency_LengthOptions)
      #   if (input$lengthFrequency_LengthOptions == "Length_inch") 1 else 10
      # })
      
      
      #inputs to return to parent module
      return(
        list(
          "lengthFrequency_LengthOptions" = reactive({ input$lengthFrequency_LengthOptions }),
          "lengthFrequencyBinwidthOptions" = reactive({ input$lengthFrequencyBinwidthOptions }), 
          "validBinWidth" = reactive({ iv$is_valid() })
        )
      )
    }
  )
}