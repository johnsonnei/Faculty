#'Cleaning the Data 1.1 - By Year
#'
#'Cleans the data for the Year people received their degrees by recursively deleting frames in the 3rd column that is not a year, eventually shifting all years not in the 3rd column to the 3rd column, while introducing NA's (which will be later removed) as to not disrupt the index of the dataframe.
#'@param data frame          The name of the data frame, Faculty (from upload)
#'@return A new dataframe (unassigned) that now has all years at which people received their degrees, in the 3rd column. Some frames may include NA's.
#'@export
cleanyr1 <- function(df) {
  result <- df
  for( i in (1:nrow(df))) {
    #set x to be the current row in the for loop we are looking at (essentially every row)
    x <- df[i,]
    #set y to be the 3rd column of the current row, the column of years people received their degrees, BA, AB, or BS
    y <- x[1,3]
    #make y a character, so we can compare them in our if statement below. Set these characters to be z
    z<-as.character(y)
    #if z starts with a 1, or a 2, assuming the faculty member received their BA, AB, or BS degree in the 1900's or 2000's
    if(substring(z,1,2) == " 1" || substring(z,1,2) == " 2"){
      #leave as is
    }
    else{
      #delete the current column
      x <- x[-3]
      len <- ncol(df)
      if (length(x) == len){
        result[i,] <- x
      }
      else{
        #because we deleted columns, to not leave holes in our dataframe, we replace the holes with NA's
        result[i,] <- c(x, rep(NA, len - length(x)))
      }
    }
  }
  return (result)
}

#'Cleaning the Data 1.2 - By Year
#'
#'Cleans the data for the Year people received their degrees by recursively deleting frames in the 3rd column that contains NA's due to the introduction of NA's when removing unwanted data in cleanyr1()
#'@param data frame          The name of the data frame, Faculty (from cleanyr1)
#'@return A new dataframe (unassigned) that now has all years at which people received their degrees, in the 3rd column. Some frames may still be blank.
#'@export
cleanyr2 <- function(df) {
  result <- df
  for( i in (1:nrow(df))) {
    #set x to be the current row in the for loop we are looking at (essentially every row)
    x <- df[i,]
    #set y to be the 3rd column of the current row, the column of years people received their degrees, BA, AB, or BS
    y <- x[1,3]
    z<-as.character(y)
    #if the 3rd column is not an NA
    if(!is.na(y)){
      #leave as is
    }
    else{
      #if the 3rd column frame is indeed an NA, then replace the entire row with NA's
      result[i,] <- rep(NA, ncol(df))
    }
  }
  #filter out all the rows that start with an NA (since we have already replaced the rows we do not want with NA's)
  result <- result %>% filter(!is.na(V1))
  return (result)
}


#'Cleaning the Data 1.3 - By Year
#'
#'Cleans the data for the Year people received their degrees by recursively deleting frames in the 3rd column that is blank.
#'@param data frame          The name of the data frame, Faculty (from cleanyr2)
#'@return A new dataframe (unassigned) that now has all years at which people received their degrees, in the 3rd column.
#'@export
cleanyr3 <- function(df) {
  result <- df
  for( i in (1:nrow(df))) {
    #set x to be the current row in the for loop we are looking at (essentially every row)
    x <- df[i,]
    #set y to be the 3rd column of the current row, the column of years people received their degrees, BA, AB, or BS
    y <- x[1,3]
    #make y a character, so we can compare them in our if statement below. Set these characters to be z
    z<-as.character(y)
    #if z starts with a 1, or a 2, assuming the faculty member received their BA, AB, or BS degree in the 1900's or 2000's
    if(substring(z,1,2) == " 1" || substring(z,1,2) == " 2"){
      #leave as is
    }
    else{
      #if z does not start with a 1 or 2 (in our case we want those that are blank because those are the only frames left that do not contain the year) replace the entire row with NA's
      result[i,] <- rep(NA, ncol(df))
    }
  }
  #filter out all the rows that start with an NA
  result <- result %>% filter(!is.na(V1))
  return (result)
}

