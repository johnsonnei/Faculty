#'Average Age
#'
#'Finds the average age of the Faculty
#'@param data frame          The name of the clean data frame, Faculty
#'@return The average age of the faculty
#'@export
AverageAge <- function(df){
  BaYear<-df
  BaYear <- as.numeric(as.character(BaYear$V3))
  FacultyAge <- 2015 - BaYear + 21
  AveAge <- mean(FacultyAge)
  print(AveAge)
}

#'Maximum Age of the Faculty
#'
#'Finds the Maximum age within the faculty
#'@param data frame          The name of the clean data frame, Faculty
#'@return The Maximum age in the faculty
#'@export
MaxAge<-function(df){
  BaYear<-df
  BaYear <- as.numeric(as.character(BaYear$V3))
  FacultyAge <- 2015 - BaYear + 21
  max(FacultyAge)
}

#'Minimum Age of the Faculty
#'
#'Finds the Minimum age within the faculty
#'@param data frame          The name of the clean data frame, Faculty
#'@return The Minimum age in the faculty
#'@export
MinAge<-function(df){
  BaYear<-df
  BaYear <- as.numeric(as.character(BaYear$V3))
  FacultyAge <- 2015 - BaYear + 21
  min(FacultyAge)
}

#'Median Age of the Faculty
#'
#'Finds the Median age within the faculty
#'@param data frame          The name of the clean data frame, Faculty
#'@return The Median age in the faculty
#'@export
MedianAge<-function(df){
  BaYear<-df
  BaYear <- as.numeric(as.character(BaYear$V3))
  FacultyAge <- 2015 - BaYear + 21
  median(FacultyAge)
}

#'Summary of the Faculty
#'
#'Displays the minimum, 1st quartile, median, 3rd quartile, and maximum of the ages of the faculty.
#'@param data frame          The name of the clean data frame, Faculty
#'@return The the summary data of the ages of the faculty
#'@export
Summary<-function(df){
  BaYear<-df
  BaYear <- as.numeric(as.character(BaYear$V3))
  FacultyAge <- 2015 - BaYear + 21
  summary(FacultyAge)
}

#'Rangee of the ages of the Faculty
#'
#'Finds the range of the ages within the faculty
#'@param data frame          The name of the clean data frame, Faculty
#'@return The range age in the faculty
#'@export
AgeRange<-function(df){
  MaxAge(df)-MinAge(df)
}
