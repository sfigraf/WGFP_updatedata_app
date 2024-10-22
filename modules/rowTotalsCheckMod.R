rowTotalsCheck_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    textOutput(ns("totalRows"))
    )
  
}

rowTotalsCheck_Server <- function(id, Detections) {
  moduleServer(id, function(input, output, session) {
    output$totalRows <- renderText({
      if (isTruthy(Detections)) {
        text <- c("Number of Rows:", format(nrow(Detections), big.mark = ","))
        return(text)
      }
    })
  })
}