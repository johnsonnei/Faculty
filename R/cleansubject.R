#'Cleaning the Data 3.0 - By Department
#'
#'Cleans the data for the Department the faculty is a member of.
#'@param data frame          The name of the data frame, Faculty (from cleanname)
#'@return A new dataframe (unassigned) that now has department of the Faculty member, cleaning out unnecessary symbols and terminology that had previously existed in the data.
#'@export
cleansubject<-function(df){
  result<-df
  #We must set the column to be characters so that we may use the string pattern matching function below
  result[,2] <- sapply(result[,2],as.character)
  #create a vector of names of the Departments that we will replace the current data with.
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
    #set x to be the current row in the for loop we are looking at (essentially every row)
    x <- df[i,]
    #set y to be the 2nd column of the current row, the column of the Departments of the faculty.
    y <- x[1,2]
    #make sure the extracted frame is still character class
    z<-as.character(y)
    #run the for loop for all the names in our vector of department names
    for (g in names) {
      #if any of the names in our vector matches a word in the frame we are comparing with
      if(length(grep(g, z))>0){
        #replace that frame with the name in our vector that had matched with a word in the frame
        result[i,2]<-g
      }
      #otherwise leave as is
      else{}
    }
  }
  return(result)
}
