library("RSNNS")        
source("titanic_predict.R")
errors<-c()
forward=function(model, data){
  predict_data=predict(model,data)
  return(predict_data)
}

for (i in 2:6) {
  train=function(data, target){
  model <- mlp(   x=data,                                          #input data for training
                  y=target,                                          #output data (targets) for training
                  size=i,                                         #number of neurons in the hidden layer
                  learnFunc="Std_Backpropagation",               #type of learning
                  learnFuncParams=c(0.1),                         #paramenters of the learning function (eta)
                  maxit=200)                                     #maximum number of iterations
                                            
  return(model)
  }
  error <- analize(train, forward)
  errors <- rbind(errors, error)
  
}
plot(errors[,1],type="l")
lines(errors[,2],col="red")

