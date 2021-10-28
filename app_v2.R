library(shiny)
#library(data.table)
library(tidyverse)
library(lubridate)
library(shinycssloaders)
library(DT)

#changes max upload size to 30 mb
options(shiny.maxRequestSize=60*1024^2)

#brings in data cleaning function
source("clean_data_function.R")


ui <- fluidPage(
    titlePanel("WGFP Data Uploads"),
    sidebarLayout(
        sidebarPanel(
            
            fileInput("file1", "Choose New Detection File"),
            
            fileInput("file2", "Choose Previous data File"),
            
            # once new detection file is uploaded, this panel appears
            conditionalPanel(condition = "output.fileUploaded == true",
                             downloadButton(outputId = "download1", label = "Save New Detections as CSV"),
                             tags$hr(),
                             downloadButton(outputId = "download2", label = "Save Combined Data")
                             ) #end of conditional panel
        ), #end of sidebar
        mainPanel(tabsetPanel(
            tabPanel("How to Use",
                     includeHTML(paste0("www/", "WGFP_data_uploads_about.html"))),
            tabPanel("New Detections",
                     withSpinner(DT::dataTableOutput("new_data_contents"))),
            tabPanel("Previous Detections",
                     withSpinner(DT::dataTableOutput("previousdata"))),
            tabPanel("Combined Data",
                     withSpinner(DT::dataTableOutput("combineddata")))
            
            
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
    
    #brings in clean_txt function and cleans .txt file jumble
    cleaned_data <- reactive({
        clean_txt(new_data())
    })
    
    #displays new cleaned data
    output$new_data_contents <- renderDataTable({
        cleaned_data()
    })
    
    # Uploading Previous Detections
    ##  Error in <Anonymous>: 'data' must be 2-dimensional (e.g. data frame or matrix)
    #solved by doing date data wrangling outside of a reactive context. ie in a function. Now that all CSV 
    previous_detections1 <- reactive({
        inFile <- input$file2
        
        if (is.null(inFile))
            return(NULL)
        #brings in uploaded file with all columns as characters except DTY column
        previous_detections <- read_csv(inFile$datapath, col_types = "cDccccccccc")
        
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
            write_csv(cleaned_data(), file)
            
            
        }
    )
    

# Combining Files ---------------------------------------------------------
    updated_data <- reactive({
        new_x <- bind_rows(previous_detections1(),cleaned_data())
        
    })
    
    output$combineddata <- renderDataTable({
        
        updated_data()
    })
    # Saving New Combined File
    
    output$download2 <- downloadHandler(
        
        
        filename = 
            function() {
                paste("WGFP_Raw", str_sub(input$file1,-13,-5), ".csv", sep = "")
            }
        ,
        content = function(file) {
            write_csv(updated_data(), file,  progress = TRUE)
        }
    )
    
    
    
    
}

shinyApp(ui, server)
