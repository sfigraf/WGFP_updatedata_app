library(shiny)
library(tidyverse)
library(lubridate)
library(DT)


source("clean_data_function.R")

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      # accept = c(
                      #     "text/csv",
                      #     "text/comma-separated-values,text/plain",
                      #     ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
        ),
        mainPanel(
            tableOutput("contents")
        )
    )
)

server <- function(input, output) {
    
    new_data <- reactive({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        read.delim(inFile$datapath, sep = " ", na.strings=c("", "NA"),
                   #skip = 5,
                   header= FALSE)    })
    
    cleaned_data <- reactive({
        clean_txt(new_data())
    })
    
    output$contents <- renderTable({
        cleaned_data()
    })
}

shinyApp(ui, server)
