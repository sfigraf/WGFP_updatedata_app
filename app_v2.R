library(shiny)
#library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)
library(shinycssloaders)
library(DT)
library(plotly)
## Idea: make variable at beginning for infIle1 and inFile2 showing biomark vs WGFP then 
# incorporate that variable in all the If statements so the code is just more concise

#changes max upload size to 600 mb
options(shiny.maxRequestSize=600*1024^2)

#brings in data cleaning functions
for (i in list.files("./functions/")) {
  source(paste0("./functions/",i))
}



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
                     withSpinner(DT::dataTableOutput("combineddata"))),
            tabPanel("QAQC ",
                     withSpinner(DT::dataTableOutput("problem_times")),
                     withSpinner(plotlyOutput("plot1")), #newdatamarkertagPlot
                     withSpinner(plotlyOutput("plot2"))) #combined data hourly marker tags
            
            
        ), #end of tabset panel
        
        ) #end of main panel
    )
)

server <- function(input, output) {
    
    cleaned_data <- reactive({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file1
        
        
        if (is.null(inFile))
            return(NULL)
        # if file ends with .txt, it gets cleaned. if it's a biomark one ending in .xlsx, it just gets brought in 
        if (endsWith(inFile$name, ".TXT")) {
            # print(TRUE)
            new_stationaryFile <- read.delim(inFile$datapath, sep = " ", na.strings=c("", "NA"),
                       #skip = 5,
                       header= FALSE)
            #cleans txt file if it is a txt file that was uploaded
            cleanedTxt <- clean_txt(new_stationaryFile)
            #cleans timestamps, filters dates
            cleanedStationary <- cleanStationary(cleanedTxt)
            
            return(cleanedStationary)
            

        } else if (endsWith(inFile$name, ".xlsx")) {
            #reads excel file if it was a excel file
            biomark <- read_excel(inFile$datapath, sheet = "Downloaded Tag IDs")
            biomark$`Scan Date` <- as_date(mdy(biomark$`Scan Date`))
            #Error in <Anonymous>: 'data' must be 2-dimensional (e.g. data frame or matrix)
            #solved by wrapping biomark in return statement
            return(biomark)
            
        }
        
        else if (endsWith(inFile$name, ".csv")) {
            #reads csv if it was a previously cleaned stationary csv 
            cleaned_stationary <- read_csv(inFile$datapath, col_types = "ccccccccccc")
            cleaned_stationary <- cleaned_stationary %>%
                mutate(DTY = ifelse(str_detect(DTY, "/"), 
                                    as.character(mdy(DTY)), 
                                    DTY))
            #biomark$`Scan Date` <- as_date(mdy(biomark$`Scan Date`))
            
            return(cleaned_stationary)
            
        }
         
        })
    
    
    #makes a fileUploaded output option to return back to conditional panel for saving csv
    output$fileUploaded <- reactive({
        return(!is.null(cleaned_data()))
    })
    
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    
    
    #displays new cleaned data
    output$new_data_contents <- renderDT({
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
        ## Parsing error with Dates in Biomark File solved jst by re-adding the last B1 and B2 detection files
        if (grepl("Biomark", inFile$name)) { #(str_detect(inFile, "Biomark"))
            previous_detections <- read_csv(inFile$datapath, col_types = "Dcccccccccc")
        } else if (str_detect(inFile$name, "WGFP|Stationary")) { #if it's not a biomark file, then it has to be related to Stationary stuff so it will be brought in this way
            
          previous_detections <- readRDS(inFile$datapath) #, col_types = "ccccccccccc"
            previous_detections$EFA <- as.character(previous_detections$EFA)
            # previous_detections <- previous_detections %>%
            #     mutate(DTY = ifelse(str_detect(DTY, "/"), 
            #                         as.character(mdy(DTY)), 
            #                         DTY))
        }
        return(previous_detections)
        
        
    })
    
    output$previousdata <- renderDT({
      
      datatable(
        previous_detections1(),
        options = list(
          #statesave is restore table state on page reload
          stateSave =TRUE,
          pageLength = 10, info = TRUE, lengthMenu = list(c(10,25, 50, 100, 200), c("10", "25", "50","100","200")),
          #dom = 'Blrtip', #had to add 'lowercase L' letter to display the page length again
          language = list(emptyTable = "Previous Detections not uploaded")
        )
      )
    })
    

# Plot Marker Reactives ---------------------------------------------------

    plot_ready_new_data <- reactive({
        
        #returns a file of only marker tags for QAQC hourly plot
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        #brings in uploaded file with all columns as characters except DTY column
        ## Parsing error with Dates in Biomark File solved jst by re-adding the last B1 and B2 detection files
        if (endsWith(inFile$name, ".TXT") | endsWith(inFile$name, ".csv"))  {
            cleaned_new_time_data <- cleaned_data() %>%
                filter(
                    str_detect(TAG, "^0000000")
                ) #%>%
                #this is the same process that all_detections goes through
                # mutate(Scan_Time1 = case_when(str_detect(ARR, "AM") & str_detect(ARR, "^12:") ~ hms(ARR) - hours(12),
                #                               str_detect(ARR, "PM") & str_detect(ARR, "^12:") ~ hms(ARR),
                #                               
                #                               str_detect(ARR, "AM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR),
                #                               str_detect(ARR, "PM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR) + hours(12),
                #                               #if it doesn't detect PM or AM just do hms(ARR)
                #                               str_detect(ARR, "PM|AM") == FALSE ~ hms(ARR)),
                #        Scan_Time2 = as.character(as_datetime(Scan_Time1)), 
                #        CleanARR = str_trim(str_sub(Scan_Time2, start = 11, end = -1))
                # ) %>%
                # 
                # select(Code, DTY, ARR, CleanARR, TRF, DUR, TTY, TAG, SCD, ANT, NCD, EFA)
            
            cleaned_new_time_data1 <- cleaned_new_time_data %>%
                mutate(hour1 = hour(as_datetime(paste(DTY, ARR)))) 
                
        } else if (endsWith(inFile$name, ".xlsx")) { 
            new_biomark1 <- cleaned_data() %>%
                filter(str_detect(`DEC Tag ID`, "^999")) %>%
                mutate(hour1 = hour(as_datetime(paste(`Scan Date`, `Scan Time`)))) %>%
                rename(SCD = `Reader ID`)
        }
    })
    
    plot_ready_previous_data <- reactive({
        
        inFile <- input$file2
        
        if (is.null(inFile))
            return(NULL)
        #brings in uploaded file with all columns as characters except DTY column
        ## Parsing error with Dates in Biomark File solved jst by re-adding the last B1 and B2 detection files
        if (str_detect(inFile$name, "Biomark")) {
            
            problem_times11 <- previous_detections1() %>%
                filter(str_length(`Scan Time`) < 8) %>%
                mutate(month111 = month(`Scan Date`)) 
            # this is a marker tag file
            previous_detections2 <- previous_detections1() %>%
                distinct(.keep_all = TRUE) %>%
                mutate(hour1 = hour(as_datetime(paste(`Scan Date`, `Scan Time`)))) %>%
                filter(
                    str_detect(`DEC Tag ID`, "^999")
                ) %>%
                rename(SCD = `Reader ID`)
            
        } else if (str_detect(inFile$name, "WGFP")) { #if it's not a biomark file, then it has to be related to Stationary stuff
            problem_times11 <- previous_detections1() %>%
                filter(str_length(ARR) < 8) %>%
                mutate(month111 = month(DTY)) 
            
            #filters to get just marker tags then corrects ARR time
            previous_detections2 <- previous_detections1() %>%
                filter(
                    str_detect(TAG, "^0000000")
                ) #%>%
                # #this is the same process that all_detections goes through
                # mutate(Scan_Time1 = case_when(str_detect(ARR, "AM") & str_detect(ARR, "^12:") ~ hms(ARR) - hours(12),
                #                               str_detect(ARR, "PM") & str_detect(ARR, "^12:") ~ hms(ARR),
                #                               
                #                               str_detect(ARR, "AM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR),
                #                               str_detect(ARR, "PM") & str_detect(ARR, "^12:", negate = TRUE) ~ hms(ARR) + hours(12),
                #                               #if it doesn't detect PM or AM just do hms(ARR)
                #                               str_detect(ARR, "PM|AM") == FALSE ~ hms(ARR)),
                #        Scan_Time2 = as.character(as_datetime(Scan_Time1)), 
                #        CleanARR = str_trim(str_sub(Scan_Time2, start = 11, end = -1))
                # ) %>%
                
                #select(Code, DTY, ARR, CleanARR, TRF, DUR, TTY, TAG, SCD, ANT, NCD, EFA)
            
            previous_detections2 <- previous_detections2 %>%
                mutate(hour1 = hour(as_datetime(paste(DTY, ARR))))
        }
        
        previous_det_list <- list(
            "problem_times" = problem_times11,
            "plotready_prev" = previous_detections2
        )
        
        return(previous_det_list)
        
            
    })
    
    output$problem_times <- renderDT({
        datatable(plot_ready_previous_data()$problem_times,
                  caption = h4("Problem Times: Previous Detections, Time got read in wrong (number of characters in string under 8). This is a good thing if this df is empty") )
    })
    
    
# PlotOutputs -------------------------------------------------------------

    #QAQC new detections
    output$plot1 <- renderPlotly({
      if(is.null(plot_ready_new_data())){
        emptydf <- data.frame(X = c(0,1), Y = c(0,1))
        emptydf %>%
          ggplot(aes(x = X)) +
          
          theme_classic() +
          labs(title = "Empty Plot: No New Detections Uploaded" 
               )
        
      } else {
        Markers_only_new <- plot_ready_new_data() %>%
          ggplot(aes(x = hour1, fill = SCD)) +
          geom_bar(stat = "Count") +
          theme_classic() +
          labs(title = "Hourly Marker Tags by Site: New Detections", 
               x = "Hour of Day")
        
        
        ggplotly(Markers_only_new)
      }
        
    })    
    #QAQC Previos detections
    output$plot2 <- renderPlotly({
      if(is.null( plot_ready_previous_data()$plotready_prev)){
        emptydf <- data.frame(X = c(0,1), Y = c(0,1))
        emptydf %>%
          ggplot(aes(x = X)) +
          theme_classic() +
          labs(title = "Empty Plot: No Previous Detections Uploaded" 
          )
        
      } else {
        Markers_only_previous <- plot_ready_previous_data()$plotready_prev %>%
          
          ggplot(aes(x = hour1, fill = SCD, text = as.character(hour1))) +
          geom_bar(stat = "Count") +
          theme_classic() +
          labs(title = "Hourly Marker Tags by Site: Previous Detections", 
               x = "Hour of Day")
        
        
        ggplotly(Markers_only_previous)
      }
        
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
        combinedDetections <- bind_rows(previous_detections1(), cleaned_data())
        combinedDetections$EFA <- as.numeric(combinedDetections$EFA)
        #delete duplicate rows
        combinedDetections <- combinedDetections %>%
          distinct()
        return(combinedDetections)
        
    })
    
    output$combineddata <- renderDT({
      
        updated_data()
    })
    # Saving New Combined File
    
    output$download2 <- downloadHandler(
        
        
        filename = 
            function() {
                inFile <- input$file1
                if (endsWith(inFile$name, ".TXT") | endsWith(inFile$name, ".csv")) {
                    paste("WGFP_Raw", str_sub(inFile,-13,-5), ".rds", sep = "")
                    
                } else if (endsWith(inFile$name, ".xlsx")) {
                    paste("Biomark_Raw", str_sub(inFile,-14,-6), ".csv", sep = "")
                    
                }
            }
        ,
        content = function(file) {
          inFile <- input$file2
          if(endsWith(inFile$name, ".rds")){
            
            saveRDS(updated_data(), file = file)
          } else if(endsWith(inFile$name, ".csv")){
            write_csv(updated_data(), file,  progress = TRUE)
          }
        }
    )
    
    
    
    
}

shinyApp(ui, server)
