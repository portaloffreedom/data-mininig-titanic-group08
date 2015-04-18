library("RSNNS")        
source("titanic_predict.R")
errors<-c()
forward=function(model, data){
  predict_data=predict(model,data)
  return(predict_data)
}

i_values <- 2:9
for (i in i_values) {
  train=function(data, target){
  model <- mlp(   x=data,                                          #input data for training
                  y=target,                                          #output data (targets) for training
                  size=i,                                         #number of neurons in the hidden layer
                  learnFunc="Std_Backpropagation",               #type of learning
                  learnFuncParams=c(0.1),                         #paramenters of the learning function (eta)
                  maxit=500)                                     #maximum number of iterations
                                            
  return(model)
  }
  error <- analize(train, forward)
  errors <- rbind(errors, error)
  
}
plot(c(min(i_values),max(i_values)), c(min(errors), max(errors)) ,type="n")
lines(i_values, errors[,1], col="black")
lines(i_values, errors[,2], col="red")

