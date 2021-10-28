library(tidyverse)
#skips the unnecessary part of the txt file, registers columsn separated by spaces, turns blank entries into NA's (important for later), and doesn't include any column names
x <- read.delim("RB\\CR_RB_A2_20210729.txt", sep = " ", na.strings=c("", "NA"),skip = 4, header= FALSE
                #col.names = c("Code",DTY","ARR", "TRF","DUR","TTY","TAG","SCD","ANT","NCD","EFA")
  )

y <- clean_txt(x)                
                                              
x1 <- x %>%
  select(26:ncol(x))

x1 %>%
  is.na %>%
  `!`


for (entry in 1:ncol(x1)) {
  if (!is.na(entry)) {
    print(entry)
  }
}

x2 <- head(x1)

x2[, 1:ncol(x2)] <- as.character(x2[, 1:ncol(x2)])


for (i in 1:ncol(x2)) {
  #print(paste(i))
  for (i2 in 1:nrow(x2)) {
    print(x2[i,i2])
    
    # if (!is.na(x1[column,row])) {
    #   print(paste(x1[column,row]))
    # }
  }
  
}

# for (row in 1:nrow(x2)) {
#   if (!is.na(row)) {
#     print(paste(TRUE)
#     }
#   } 
  
for (row in 1:nrow(x2)) {
  if(!is.na(x2[row,])) {
    print(paste("row"))
   }
}


x1[1,1]

for (i in 1:nrow())


if (!is.na(x1[1,1]) == TRUE) {
  print(paste("True"))
}


x <- read.delim("CR_RB_A2_20210831.txt", sep = " ", na.strings=c("", "NA"),
                skip = 5, header= FALSE)
                
x1 <- x %>%
  select(26:ncol(x))
x2 <- head(x1)


# for just ncd efa cols ---------------------------------------------------

x1 <- x %>%
  select(26:ncol(x))

not_any_na <- function(x) all(!is.na(x))
#making new df for just the NCD and EFA columns
newdf <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(newdf) <- c("NCD","EFA")
#for loop goes through, subsets each row,
#filters out entries that don't have data, 
#adds the rows to the newdf
for (i in 1:nrow(x1)) {
  
  c <- x1[i,]
  #print(c)
  c1 <- c %>%
    select(where(not_any_na)) 
  
  
  #print(c1)
  
  #if the subset wasn't just filled with NA's/had a length >0, add to newdf 
  if (length(c1)>0){
    colnames(c1) <- c("NCD","EFA")
    newdf <- bind_rows(newdf,c1)
  }
  
}
  

c <- x2[2,]
c1 <- c %>%
  select(where(not_any_na))

x3 <- bind_rows(newdf, c1)
# newdf <- data.frame (NCD = c1[1,1],
#                      EFA = c1[1,2])


# for all data ------------------------------------------------------------
x <- read.delim("RB\\CR_RB_A2_20210831.txt", sep = " ", na.strings=c("", "NA"),skip = 5, header= FALSE)
                
#x3 <- x[1:20,]
not_any_na <- function(x) all(!is.na(x))
# not_any_blank <- function(x) all()
#making new df for just the NCD and EFA columns
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
for (i in 1:nrow(x)) {
  
  c <- x[i,]
  #print(c)
  c1 <- c %>%
    select(where(not_any_na)) 
  
  
  #print(c1)
  
  #if the subset wasn't just filled with NA's/had a length ==11, add to newdf 
  if (length(c1)==11){
    colnames(c1) <- c("Code","DTY","ARR", "TRF","DUR","TTY","TAG","SCD","ANT","NCD","EFA")
    c1[, 1:ncol(c1)] <- as.character(c1[, 1:ncol(c1)])
    
    newdf <- bind_rows(newdf,c1)
  }
  
}

newdf2 <- newdf[2:nrow(newdf),]

## need to remove dummy data
# c <- x3[2,]
# #print(c)
# c1 <- c %>%
#   select(where(not_any_na))
# 
# colnames(c1) <- c("Code","DTY","ARR", "TRF","DUR","TTY","TAG","SCD","ANT","NCD","EFA")
# c1[, 1:ncol(c1)] <- as.character(c1[, 1:ncol(c1)])
# newdf[, 1:ncol(newdf)] <- as.character(newdf[, 1:ncol(newdf)])
# # col
# # newdf <- bind_rows(newdf,c1)
# # 
# # 
# # c2 <- c1[,1]
# # c2 <- c1
# newdf2 <- newdf[,9:ncol(newdf)]
# c2 <- c1[,9:ncol(c1)]
# c3 <- bind_rows(c2,newdf2)

c <- x[3,]
c1 <- c %>%
  select(where(not_any_na)) 


#print(c1)

#if the subset wasn't just filled with NA's/had a length >0, add to newdf 
if (length(c1)==11){
  colnames(c1) <- c("Code","DTY","ARR", "TRF","DUR","TTY","TAG","SCD","ANT","NCD","EFA")
  c1[, 1:ncol(c1)] <- as.character(c1[, 1:ncol(c1)])
  
}

clean_txt(x)


# combining dfs -----------------------------------------------------------

new_data <- read.delim("RB\\CR_RB_A2_20210729.txt", sep = " ", na.strings=c("", "NA"),
                #skip = 4, 
                header= FALSE
                #col.names = c("Code",DTY","ARR", "TRF","DUR","TTY","TAG","SCD","ANT","NCD","EFA")
)

new_data1 <- clean_txt(new_data)
new_data1$DTY <- as_date(ymd(new_data1$DTY))
# Master dataframe date/column wrangling
previous_data <- read_csv("WGFP_Raw_20210505_3.csv", col_types = "cDccccccccc")
#previous_data = previous_data[,-1]
# previous_data %>%
#   mutate(DTY <- mdy(DTY))



previous_data$DTY <- as_date(mdy(previous_data$DTY))

#new_x <- bind_rows(previous_data,new_data1)

#new_x$DTY <- as_date(mdy(new_x$DTY))
write_csv(previous_data, "WGFP_Raw_20210505_3.csv")
