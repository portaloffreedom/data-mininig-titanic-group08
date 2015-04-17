setwd("/home/matteo/Documenti/VU/Data Mining/Assigment 1/Titanic/")
data_orig <- read.csv("train.csv", header=TRUE)

## preprocessing part
data <- data_orig
data <- subset(data, select= -PassengerId)
data <- subset(data, select= -Name)
data <- subset(data, select= -Cabin)
data <- subset(data, select= -Ticket)

data$relatives = data$SibSp + data$Parch
data <- subset(data, select= -SibSp)
data <- subset(data, select= -Parch)

median_age <- median(data$Age, na.rm = TRUE)
data$Age[is.na(data$Age)] <- median_age 
