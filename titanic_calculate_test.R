library("RSNNS")
source("titanic_load_data.R")


train=function(data, target){
  model <- mlp(   x=data,                                          #input data for training
                  y=target,                                          #output data (targets) for training
                  size=5,                                         #number of neurons in the hidden layer
                  learnFunc="Std_Backpropagation",               #type of learning
                  learnFuncParams=c(0.1),                         #paramenters of the learning function (eta)
                  maxit=500)                                     #maximum number of iterations
  
  return(model)
}


forward=function(model, data){
  predict_data=predict(model,data)
  return(predict_data)
}


## main execution part
data <- loadData("train.csv", "test.csv")
trainData <- data[[1]]
testData <- data[[2]]

target_train <- trainData$Survived

train_x <- cbind(trainData$Sex, trainData$Age, trainData$Fare, trainData$relatives)
test_x <- cbind(testData$Sex, testData$Age, testData$Fare, testData$relatives)

model <- train(train_x, target_train)

result_test <- forward(model, test_x)

result <- ifelse(result_test < .5, 0, 1)

dataTest_ <- read.csv("test.csv", header=TRUE)
resultCSV <- cbind(dataTest_$PassengerId, result)
colnames(resultCSV) <- c("PassengerId","Survived")
write.csv(resultCSV, "submission.csv", row.names = FALSE)
