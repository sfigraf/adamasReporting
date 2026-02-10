lengthWeightInputs_UI <- function(id, class = "green-row") {
  ns <- NS(id)
  tagList(
    fluidRow(class = class,
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
}

lengthWeightInputs_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      return(
        list(
          "lengthWeight_WeightOptions" = reactive({ input$lengthWeight_WeightOptions }),
          "lengthWeight_LengthOptions" = reactive({ input$lengthWeight_LengthOptions })
        )
      )
      
    }
  )
}