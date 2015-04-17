#setwd("/home/matteo/Documenti/VU/Data Mining/Assigment 1/Titanic/")

summed_squared_difference <- function(result,target) {
  difference <- result-target
  return(sum(difference*difference))
}

analize <- function(train, fwd) {
  source("titanic_load_data.R")
  
  cross_sections <- 10
  crossSelection <- sample(1:cross_sections, size=nrow(data), replace=TRUE)
  
  error_train <- numeric(cross_sections)
  error_test <- numeric(cross_sections)
    
  for (i in 1:cross_sections) {
    selection <- crossSelection == i
    train <- data[!crossSelection,]
    test <- data[crossSelection,]
    
    target_train <- train$Survived
    target_test <- test$Survived
    
    train_x <- c(train$Sex, train$Age, train$Fare, train$relatives);
    test_x <- c(test$Sex, test$Age, test$Fare, test$relatives);
    
    v <- train(train_x)
    
    result_train <- fwd(v, train_x)
    result_test <- fwd(v, test_x)
    
    error_train[i] <- summed_squared_difference(result_train, target_train)
    error_test[i] <- summed_squared_difference(result_test, target_test)
  }
  
  print(mean(error_train))
  print(mean(error_test))
}

