runReport_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("reportBuilderButton"), "Run Report", style = "height: 38px;"),
    hr()
  )
}

runReport_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
}