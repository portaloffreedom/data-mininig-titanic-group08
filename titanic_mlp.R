library("RSNNS")        
source("titanic_predict.R")
train=function(data, target){
  model <- mlp(   x=data,                                          #input data for training
                  y=target,                                          #output data (targets) for training
                  size=1,                                         #number of neurons in the hidden layer
                  learnFunc="Std_Backpropagation",               #type of learning
                  learnFuncParams=c(0.1),                         #paramenters of the learning function (eta)
                  maxit=100)                                     #maximum number of iterations
                                            
  return(model)
}
forward=function(model, data){
  predict_data=predict(model,data)
  return(predict_data)
}

analize(train, forward)
