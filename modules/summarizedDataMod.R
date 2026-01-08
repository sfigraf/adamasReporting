# sql2<-"Select * from CurrentSummary where year(SampleDate)=2025"
# SummarizedData<- dbGetQuery(CPW_AqDatAnalysis, sql2)
data <- tbl(CPW_AqDatAnalysis, "CurrentSummary")
allYears <- data %>%
  distinct(year(SampleDate)) %>%
  pull()

summarizedData_UI <- function(id) {
  ns <- NS(id)
  tagList(

    sidebarLayout(
      sidebarPanel(
        virtualSelectInput(ns("yearsSearch"),
                           label = "Year:",
                           choices = sort(allYears),
                           search = TRUE,     
                           autoSelectFirstOption = FALSE
        ),

        # virtualSelectInput(ns("waterNameSearch"),
        #   label = "Water Name:",
        #   choices = allDistinctWaters,
        #   search = TRUE,
        #   autoSelectFirstOption = FALSE
        # ),
        # uiOutput(ns("yearsSearchUI")),


        actionButton(ns("queryButton"),
                     label = "Render Data", width = "100%")

      ),

      # Show a plot of the generated distribution
      mainPanel(
        DTOutput(ns("currentSummaryData")),
        downloadData_UI(ns("downloadCurentSummaryData"))
      )
    )



  )
}

summarizedData_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      currentSummaryDataToDisplay <- eventReactive(input$queryButton,ignoreNULL = TRUE,{
        data <- data %>%
          filter(year(SampleDate)==input$yearsSearch)
        
        data <- as.data.frame(data)
        #columns in this db are "blobs" type which are found in DBs I guess. this converts them to character type and allows DT to display them
        data1 <- data %>%
          mutate(across(where(~inherits(., "blob")), 
                        ~sapply(., function(x) paste(as.character(x), collapse = ""))))
        
        return(data1)
      })
      
      output$currentSummaryData <- renderDT(server = TRUE, {
        #dummy df
        # n_rows <- 100
        # n_cols <- 110
        # 
        # # Create the data using a matrix for speed
        # mat <- matrix(runif(n_rows * n_cols), nrow = n_rows, ncol = n_cols)
        # 
        # # Convert to dataframe and name columns
        # df <- as.data.frame(mat)
        # colnames(df) <- paste0("Column_", 1:n_cols)
        
        datatable(currentSummaryDataToDisplay(),
                  rownames = FALSE,
                  extensions = c('Buttons'),
                  #for slider filter instead of text input
                  filter = 'top',
                  options = list(
                    pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
                    dom = 'lfrtip', #had to add 'lowercase L' letter to display the page length again #errorin list: arg 5 is empty because I had a comma after the dom argument so it thought there was gonna be another argument input
                    language = list(emptyTable = "Enter inputs and press Render Table")
                  )
        )
      })
      
      downloadData_Server("downloadCurentSummaryData", currentSummaryDataToDisplay(), "CurrentSummaryData")

    }
  )
}