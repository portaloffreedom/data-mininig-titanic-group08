#setwd("/home/matteo/Documenti/VU/Data Mining/Assigment 1/Titanic/")
source("titanic_load_data.R")

summed_squared_difference <- function(result,target) {
  difference <- result-target
  return(mean(difference*difference))
}

analize <- function(train, fwd) {
  data <- loadData("train.csv")
  
  cross_sections <- 10
  crossSelection <- sample(1:cross_sections, size=nrow(data), replace=TRUE)
  
  error_train <- numeric(cross_sections)
  error_test <- numeric(cross_sections)
    
  for (i in 1:cross_sections) {
    selection <- crossSelection == i
    train <- data[!selection,]
    test <- data[selection,]
    
    target_train <- train$Survived
    target_test <- test$Survived
    
    train_x <- cbind(train$Sex, train$Age, train$Fare, train$relatives);
    test_x <- cbind(test$Sex, test$Age, test$Fare, test$relatives);
    
    v <- train(train_x, target_train)
    
    result_train <- fwd(v, train_x)
    result_test <- fwd(v, test_x)
    
    error_train[i] <- (summed_squared_difference(result_train, target_train))
    error_test[i] <- (summed_squared_difference(result_test, target_test))
  }
  
  error = c(mean(error_train), mean(error_test))
  print(error[1]) #mean(error_train)
  print(error[2]) #mean(error_test)
  
  return(error)
}

