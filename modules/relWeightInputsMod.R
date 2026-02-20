relWeightInputs_UI <- function(id, class = "green-row") {
  ns <- NS(id)
  tagList(
    fluidRow(class = class, 
             #since this input is used so much, make this a function
             column(6,
                    radioButtons(
                      ns("relativeWeight_LengthOptions"),
                      label = "Length Display",
                      choiceNames = c("Millimeters", "Inches"),
                      #values need to match column names
                      choiceValues = c("Length_mm", "Length_inch")
                    )
             )
    )
  )
}

relWeightInputs_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      return(
        list(
          "relativeWeight_LengthOptions" = reactive({ input$relativeWeight_LengthOptions })
        )
      )
      
    }
  )
}