#'Histogram of the Ages
#'
#'Makes and displays the Histogram of the ages of the faculty
#'@param data frame          The name of the clean data frame, Faculty
#'@return The histogram of the age of the faculty
#'@export
AgeHistogram <- function(df){
  BaYear<-df
  BaYear <- as.numeric(as.character(BaYear$V3))
  FacultyAge <- 2015 - BaYear + 21
  hist(FacultyAge,main="Distribution of Age",xlab="Age")
}

#'Histogram of the Years the faculty received their degrees
#'
#'Makes and displays the Histogram of the years the faculty received their degrees
#'@param data frame          The name of the clean data frame
#'@return The histogram of the year the faculty received their degrees
#'@export
YearHistogram <- function(df){
  BaYear<-df
  BaYear <- as.numeric(as.character(BaYear$V3))
 hist(BaYear,main="Year the Faculty received their degrees",xlab="Year")
}
