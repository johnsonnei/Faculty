#'Cleaning the Data 2.0 - By Name
#'
#'Cleans the data for the Names of the faculty.
#'@param data frame          The name of the data frame, Faculty (from cleanyr3)
#'@return A new dataframe (unassigned) that now has the name of the Faculty, cleaning out unnecessary symbols that had previously existed in the data.
#'@export
cleanname<-function(df){
  result<-df
  #We must set the column to be characters so that we may use the string pattern matching function below
  result[,1] <- sapply(result[,1],as.character)
  for( i in (1:nrow(df))) {
    #set x to be the current row in the for loop we are looking at (essentially every row)
    x <- df[i,]
    #set y to be the 1st column of the current row, the column of the names of the faculty.
    y <- x[1,1]
    #make sure the extracted column is still character class
    z<-as.character(y)
    #replace all characters that is not a letter in the alphabet, a period, or a space, into nothing.
    z<-str_replace_all(z,"[^abcdefghijklmnopqqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ. ]","")
    #update the dataframe
    result[i,1]<-z
  }
  return(result)
}
