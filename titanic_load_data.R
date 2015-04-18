#setwd("~/GitHub/data-mininig-titanic-group08")
#setwd("/home/matteo/Documenti/VU/Data Mining/Assigment 1/Titanic/")
data <- read.csv("train.csv", header=TRUE)

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
data$Fare<-data$Fare/max(data$Fare)
data$relatives<-data$relatives/max(data$relatives)


