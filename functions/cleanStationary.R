#Stationary <- cleaned
cleanStationary <- function(Stationary){
  Stationary <- Stationary %>%
    mutate(TAG = gsub("\\_", "", str_trim(TAG)), 
           DTY = ifelse(str_detect(DTY, "/"),
                        as.character(mdy(DTY)),
                        DTY)
    )
  
  #cleaning timestamps for mobile and old stationary detections mainly
  #currently we are converting to periods so that it is easier to add and subtract intervals
  if (any(grepl("PM|AM", Stationary$ARR))) {
    Stationary_cleanedTime <- Stationary %>%
      mutate(ARR1 = case_when(str_detect(ARR, "AM") & str_detect(ARR, "^12:") ~ lubridate::hms(ARR) - hours(12),
                              str_detect(ARR, "PM") & str_detect(ARR, "^12:") ~ lubridate::hms(ARR),
                              
                              str_detect(ARR, "AM") & str_detect(ARR, "^12:", negate = TRUE) ~ lubridate::hms(ARR),
                              str_detect(ARR, "PM") & str_detect(ARR, "^12:", negate = TRUE) ~ lubridate::hms(ARR) + hours(12),
                              #if it doesn't detect PM or AM just do lubridate::hms(ARR)
                              str_detect(ARR, "PM|AM") == FALSE ~ lubridate::hms(ARR)),
      ) %>%
      #but that also means that as_datetime reads those as periods and doesn't play well with the 0s interval specifically
      #so we need to convert that plain "1970-01-01" to midnight
      mutate(ARR2 = as.character(as_datetime(ARR1)), 
             ARR = ifelse(ARR2 == "1970-01-01", 
                          "00:00:00",
                          str_trim(str_sub(ARR2, start = 11, end = -1))
             )
      ) %>%
      select(-c(ARR1, ARR2))
  } else{
    Stationary_cleanedTime <- Stationary %>%
      mutate(ARR = sub("\\.\\d+", "", ARR))
  }
  
  Stationary_cleanedTime1 <- Stationary_cleanedTime %>%
    filter(nchar(DTY) == 10, 
           #this date is the first data we have for the antennas
           DTY >= as.Date("2020-08-06"), 
           Code %in% c("I", "S")) %>%
    distinct()
  
  return(Stationary_cleanedTime1)
}
