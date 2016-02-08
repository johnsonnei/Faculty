download <- function(url) {
  #url<- http://web.williams.edu/admin/registrar/catalog/bulletin2013_14.pdf
  dest <- tempfile(fileext = ".pdf")
  download.file(url, dest, mode = "wb")
}

library(dplyr)
library(stringr)

#'Uploading the Data
#'
#'Reads in a txt file in table format and creates a data frame from it, with cases corresponding to lines and variables to fileds in the file.
#'@param .txt file          The name of the txt file
#'@return The variable "Faculty", containing the data table that is imported
#'@examples upload("/Users/jnjohn1995/Downloads/faculty/Faculty/faculty13.txt")
#'@export
upload <- function(txtfile) {
   Faculty <<- read.csv(file=txtfile,header=FALSE, fill=TRUE, na.string="NA")
}


#'Cleaning the Data 1.1 - By Year
#'
#'Cleans the data for the Year people received their degrees by recursively deleting frames in the 3rd column that is not a year, eventually shifting all years not in the 3rd column to the 3rd column, while introducing NA's (which will be later removed) as to not disrupt the index of the dataframe.
#'@param data frame          The name of the data frame, Faculty (from upload)
#'@return A new dataframe (unassigned) that now has all years at which people received their degrees, in the 3rd column. Some frames may include NA's.
#'@examples Faculty<-cleanyr1(Faculty)
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
#'@examples Faculty<-cleanyr2(Faculty)
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
#'@examples Faculty<-cleanyr3(Faculty)
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




#'Cleaning the Data 2.0 - By Name
#'
#'Cleans the data for the Names of the faculty.
#'@param data frame          The name of the data frame, Faculty (from cleanyr3)
#'@return A new dataframe (unassigned) that now has the name of the Faculty, cleaning out unnecessary symbols that had previously existed in the data.
#'@examples Faculty<-cleanname(Faculty)
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



#'Cleaning the Data 3.0 - By Department
#'
#'Cleans the data for the Department the faculty is a member of.
#'@param data frame          The name of the data frame, Faculty (from cleanname)
#'@return A new dataframe (unassigned) that now has the name of the Faculty, cleaning out unnecessary symbols that had previously existed in the data.
#'@examples Faculty<-cleanname(Faculty)
#'@export
cleansubject<-function(df){
  result<-df
  result[,2] <- sapply(result[,2],as.character)
  names <- c("Physics",
             "Japanese",
             "Mathematics",
             "Computer Science",
             "Art",
             "Biology",
             "Environmental Studies",
             "Economics",
             "Geosciences",
             "Theatre",
             "English",
             "Physical Education",
             "Philosophy",
             "History of Science",
             "Romance Languages",
             "History",
             "Chemistry",
             "Music",
             "Classics",
             "Political Science",
             "Africana Studies",
             "Art History",
             "Anthropology",
             "Religion",
             "Dance",
             "Marine Sciences",
             "Russian",
             "Latina",
             "Gender and Sexuality",
             "Leadership Studies",
             "Chinese",
             "Languages",
             "American Studies",
             "Psychology",
             "French",
             "Statistics",
             "Classics",
             "Astronomy",
             "Geology",
             "German",
             "Humanities",
             "Social Sciences",
             "Arabic",
             "Latina/o Studies",
             "Athletics",
             "Spanish",
             "Sociology",
             "Neuroscience")

  for( i in (1:nrow(df))) {
    x <- df[i,]
    y <- x[1,2]
    z<-as.character(y)

    for (g in names) {
      if(length(grep(g, z))>0){

  result[i,2]<-g

      }
      else{}
    }
  }
  return(result)
}


