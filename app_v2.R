library(shiny)
library(tidyverse)
library(lubridate)
library(DT)

#changes max upload size to 30 mb
options(shiny.maxRequestSize=30*1024^2)
#brings in data cleaning function
source("clean_data_function.R")

#bringing in previous data to view
previous_detections <- read.csv("WGFP_Raw_20210505.csv")
previous_detections = previous_detections[,-1]
previous_detections$DTY <- mdy(previous_detections$DTY)

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
            #checkboxInput("header", "Header", TRUE),
            conditionalPanel(condition = "output.fileUploaded == true",
                             downloadButton(outputId = "download1", label = "Save as CSV"),
                             downloadButton(outputId = "download2", label = "Add to Master File")
                             ) #end of conditional panel
        ),
        mainPanel(tabsetPanel(
            tabPanel("How to Use",
                     includeHTML(paste0("www/", "WGFP_data_uploads_about.html"))),
            tabPanel("New Detections",
                     DT::dataTableOutput("new_data_contents")),
            tags$hr(),
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
    ##  Error in <Anonymous>: 'data' must be 2-dimensional (e.g. data frame or matrix)
    #solved by doing date data wrangling outside of a reactive context
    previous_detections1 <- reactive({
        previous_detections

    })
    
    output$previousdata <- renderDataTable({
        
        previous_detections1()
    })
    

# Downloading CSV ---------------------------------------------------------
    output$download1 <- downloadHandler(
        filename = 
            function() {
            paste(str_sub(input$file1,1,-5), ".csv", sep = "")
        }
        ,
        content = function(file) {
            write.csv(cleaned_data(), file, row.names = FALSE)
        }
    )
    

# Combining Files ---------------------------------------------------------
    updated_data <- reactive({
        new_x <- bind_rows(previous_detections1(),cleaned_data())
        
    })
    
    
    output$download2 <- downloadHandler(
        
       
        
        filename = 
            function() {
                paste("WGFP_Raw", str_sub(input$file1,-13,-5), ".csv", sep = "")
            }
        ,
        content = function(file) {
            write.csv(updated_data(), file, row.names = FALSE)
        }
    )
    
    
    
    # observeEvent(input$button1, {
    #     updated_data <- bind_rows(previous_detections1(),cleaned_data())
    #     
    # })    
    
}

shinyApp(ui, server)
