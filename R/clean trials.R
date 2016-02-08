cleanyr <- function(df) {
  result <- df
  for( i in (1:nrow(df))) {
    # local
    x <- df[i,]
    y <- x[1,3]
    z<-as.character(y)
    if(substring(z,1,2) == " 1" || substring(z,1,2) == " 2"){
      #leave as is
    }
   else{
      x <- x[-3]
      len <- ncol(df)
      if (length(x) == len){
        result[i,] <- x
      }
      else{
        result[i,] <- c(x, rep(NA, len - length(x)))
      }
   }
  }
  return (result)
}


#-------------

cleanyr2<- function(df){
  result <- df
  for( i in (1:nrow(df))) {
    # local
    x <- df[i,]

    y <- x[1,3]
    z<-as.character(y)

    if(is.na(y)){
      result[i,] <- rep(NA, ncol(df))
    }

  }
  result <- result %>% filter(!is.na(V1))
  return (result)
}



#-------------------
cleanyr3 <- function(df) {
  result <- df
  for( i in (1:nrow(df))) {
    # local
    x <- df[i,]
    y <- x[1,3]
    z<-as.character(y)
    if(substring(z,1,2) == " 1" || substring(z,1,2) == " 2"){
      #leave as it i
    }
    else{
   result[i,] <- rep(NA, ncol(df))
    }
  }
  result <- result %>% filter(!is.na(V1))
  return (result)
}

#-----------------
cleanyr4 <- function(df) {
  result <- df
  for( i in (1:nrow(df))) {
    # local
    x <- df[i,]
    y <- x[1,3]
    z<-as.character(y)
    if(substring(z,1,2) == " 1" || substring(z,1,2) == " 2"){
      #leave as is
    }
    else{
      x <- x[-3]
      len <- ncol(df)
      if (length(x) == len){
        result[i,] <- x
      }
      else{
        result[i,] <- c(x, rep(NA, len - length(x)))
      }
    }
  }
#  result <- result %>% filter(!is.na(V3))

  for( i in (1:nrow(df))){
    x <- df[i,]
    y <- x[1,3]
    z<-as.character(y)
    if(substring(z,1,2) == " 1" || substring(z,1,2) == " 2"){}
    else{
    result[i,] <- rep(NA, ncol(df))
    }
  }
result <- result %>% filter(!is.na(V1))
  return (result)
}

#----------
  cleanyr5 <- function(df) {
    result <- df
    for( i in (1:nrow(df))) {
      # local
      x <- df[i,]
      y <- x[1,3]
      z<-as.character(y)
      if(!is.na(y)){
        #leave as is
      }
      else{
        result[i,] <- rep(NA, ncol(df))
      }
    }
    result <- result %>% filter(!is.na(V1))
    return (result)
  }

#-----------

  cleanyr6 <- function(df) {
    result <- df
    for( i in (1:nrow(df))) {
      # local
      x <- df[i,]
      y <- x[1,3]
      z<-as.character(y)
      if(substring(z,1,2) == " 1" || substring(z,1,2) == " 2"){
        #leave as is
      }
      else{
          result[i,] <- rep(NA, ncol(df))
        }
    }
    result <- result %>% filter(!is.na(V1))
    return (result)
  }

  #---------
  cleansubject<-function(df){
    result<-df
    for( i in (1:nrow(df))) {
      x <- df[i,]
      y <- x[1,2]
      print(y)
      z<-as.character(y)
      #if z contains Physics, replace the string with Physics
      #if(length(grep("Japanese",z))>0){ print("found")}

      if(length(grep("Japanese",z))>0){
        #z <- str_replace(z,z,"Japanese")
        result[i,2] <- "Japanese"
      }
    }
    return(result)
  }

  #---------

AverageAge <- function(df){
  BaYear<-df
  BaYear <- as.numeric(as.character(BaYear$V3))
  FacultyAge <- 2015 - BaYear + 21
  AveAge <- mean(FacultyAge)
  print(AveAge)
}


AgeHistogram <- function(df){
  BaYear<-df
  BaYear <- as.numeric(as.character(BaYear$V3))
  FacultyAge <- 2015 - BaYear + 21
  AgeHist <- hist(FacultyAge,main="Distribution of Age",xlab="Age")
}

MaxAge<-function(df){
  BaYear<-df
  BaYear <- as.numeric(as.character(BaYear$V3))
  FacultyAge <- 2015 - BaYear + 21
  max(FacultyAge)
}

MinAge<-function(df){
    BaYear<-df
    BaYear <- as.numeric(as.character(BaYear$V3))
    FacultyAge <- 2015 - BaYear + 21
    min(FacultyAge)
}

MedianAge<-function(df){
  BaYear<-df
  BaYear <- as.numeric(as.character(BaYear$V3))
  FacultyAge <- 2015 - BaYear + 21
  median(FacultyAge)
}

Summary<-function(df){
  BaYear<-df
  BaYear <- as.numeric(as.character(BaYear$V3))
  FacultyAge <- 2015 - BaYear + 21
  summary(FacultyAge)
}

AgeRange<-function(df){
  MaxAge(df)-MinAge(df)
}
