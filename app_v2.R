library(shiny)
library(tidyverse)
library(lubridate)
library(DT)


source("clean_data_function.R")
previous_detections <- read.csv("WGFP_Raw_20210505.csv")


ui <- fluidPage(
    titlePanel("WGFP Data Uploads"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose File",
                      # accept = c(
                      #     "text/csv",
                      #     "text/comma-separated-values,text/plain",
                      #     ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            conditionalPanel(condition = "output.fileUploaded == true",
                             actionButton(inputId = "download1", label = "Save as CSV")
                             ) #end of conditional panel
        ),
        mainPanel(tabsetPanel(
            tabPanel("How to Use",
                     includeHTML(paste0("www/", "WGFP_data_uploads_about.html"))),
            tabPanel("New Detections",
                     DT::dataTableOutput("new_data_contents")),
            tabPanel("Previous Detections",
                     DT::dataTableOutput("previousdata"))
            
            
        ), #end of tabset panel
            
        ) #end of main panel
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
    #makes a fileUploaded output option to return back to conditional panel for saving csv
    output$fileUploaded <- reactive({
        return(!is.null(new_data()))
    })
    
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    cleaned_data <- reactive({
        clean_txt(new_data())
    })
    
    output$new_data_contents <- renderDataTable({
        cleaned_data()
    })
    

# Downloading CSV ---------------------------------------------------------
    # output$download1 <- downloadHandler(
    #     filename = function() {
    #         paste(input$file1, ".csv", sep = "")
    #     },
    #     content = function(file) {
    #         write.csv(datasetInput(), file, row.names = FALSE)
    #     }
    # )
    
}

shinyApp(ui, server)
