library(dplyr)
library(stringr)

#'Uploading the Data
#'
#'Reads in a txt file in table format and creates a data frame from it, with cases corresponding to lines and variables to fileds in the file.
#'@param .txt file          The name of the txt file
#'@return The variable "Faculty", containing the data table that is imported
#'@export
upload <- function(txtfile) {
  Faculty <<- read.csv(file=txtfile,header=FALSE, fill=TRUE, na.string="NA")
}
