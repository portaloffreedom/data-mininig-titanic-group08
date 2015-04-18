#setwd("~/GitHub/data-mininig-titanic-group08")
#setwd("/home/matteo/Documenti/VU/Data Mining/Assigment 1/Titanic/")

loadData <- function(filename, filename_test = NULL) {
  data <- read.csv(filename, header=TRUE)
  
  ## preprocessing part
  data <- subset(data, select= -PassengerId)
  data <- subset(data, select= -Name)
  data <- subset(data, select= -Cabin)
  data <- subset(data, select= -Ticket)
  data <- subset(data, select= -Embarked)
  
  data$relatives = data$SibSp + data$Parch
  data <- subset(data, select= -SibSp)
  data <- subset(data, select= -Parch)
  
  median_age <- median(data$Age, na.rm = TRUE)
  data$Age[is.na(data$Age)] <- median_age
  
  #normalize data
  mean_age = mean(data$Age)
  sd_age = sd(data$Age)
  
  data$Pclass<-data$Pclass-2
  data$Sex=ifelse(data$Sex=='female', 1,-1)
  data$Age<-(data$Age-mean_age)/sd_age 
  
  maxFare <- max(data$Fare)
  data$Fare<-data$Fare/maxFare
  
  maxRelatives <- max(data$relatives)
  data$relatives<-data$relatives/maxRelatives
  
  if (is.null(filename_test)) {
    return (data)
  } else {
    dataTest <- read.csv(filename_test, header=TRUE)
    
    ## preprocessing part
    dataTest <- subset(dataTest, select= -PassengerId)
    dataTest <- subset(dataTest, select= -Name)
    dataTest <- subset(dataTest, select= -Cabin)
    dataTest <- subset(dataTest, select= -Ticket)
    dataTest <- subset(dataTest, select= -Embarked)
    
    dataTest$relatives = dataTest$SibSp + dataTest$Parch
    dataTest <- subset(dataTest, select= -SibSp)
    dataTest <- subset(dataTest, select= -Parch)
    
    dataTest$Age[is.na(dataTest$Age)] <- median_age
  
    dataTest$Pclass<-dataTest$Pclass-2
    dataTest$Sex=ifelse(dataTest$Sex=='female', 1,-1)
    dataTest$Age<-(dataTest$Age-mean_age)/sd_age 
    dataTest$Fare<-dataTest$Fare/maxFare
    dataTest$Fare[is.na(dataTest$Fare)] <- median(data$Fare)
    dataTest$relatives<-dataTest$relatives/maxRelatives
    
    ret <- c("train data", "test data")
    return (list(data,dataTest))
  }
}
