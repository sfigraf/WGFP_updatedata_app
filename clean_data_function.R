clean_txt <- function(txt_input) {
  #start time of function
  start_time <- Sys.time()
  
  not_any_na <- function(x) all(!is.na(x))
  # not_any_blank <- function(x) all()
  # making new df for just the NCD and EFA columns
  # newdf <- setNames(data.frame(matrix(ncol = 11, nrow = 0)),
  #                   c("Code","DTY","ARR", "TRF","DUR","TTY","TAG","SCD","ANT","NCD","EFA"))
  #need to have at least 1 row top be able to make columns into types
  rows =1
  newdf <- data.frame(Code = character(rows), 
                      DTY = character(rows), 
                      ARR = character(rows), 
                      TRF = character(rows), 
                      DUR = character(rows), 
                      TTY = character(rows), 
                      TAG = character(rows), 
                      SCD = character(rows),
                      ANT = character(rows), 
                      NCD = character(rows),
                      EFA = character(rows))
  
  
  #colnames(newdf) <- c("Code","DTY","ARR", "TRF","DUR","TTY","TAG","SCD","ANT","NCD","EFA")
  #for loop goes through, subsets each row,
  #filters out entries that don't have data, 
  #adds the rows to the newdf
  for (i in 1:nrow(txt_input)) {
    
    c <- txt_input[i,]
    #print(c)
    c1 <- c %>%
      select(where(not_any_na)) 
    
    
    #print(c1)
    
    #if the subset wasn't just filled with NA's/had a length ==11, add to newdf 
    # # Error in names(x) <- value : 
    # 'names' attribute [11] must be the same length as the vector [1]
    #solved bc before i was just excluding lines of length 0, now I'm taking only the rows with data entries in exacdtly 11 columns
    if (length(c1)==11){
      colnames(c1) <- c("Code","DTY","ARR", "TRF","DUR","TTY","TAG","SCD","ANT","NCD","EFA")
      c1[, 1:ncol(c1)] <- as.character(c1[, 1:ncol(c1)])
      
      newdf <- bind_rows(newdf,c1)
    }
    
  }
  
  newdf2 <- newdf[2:nrow(newdf),]
  
  newdf2$DTY <- as_date(ymd(newdf2$DTY))
  
  #how long is takes
  end_time <- Sys.time()
  print(end_time-start_time)
  
  return(newdf2)
}




