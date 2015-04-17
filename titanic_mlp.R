library("RSNNS")        
source("titanic_predict.R")
train=function(data, target){
  model <- mlp(   x=data,                                          #input data for training
                  y=target,                                          #output data (targets) for training
                                            
  return(model)
}
forward=function(model, data){
  predict_data=predict(model,data)
  return(predict_data)
}

analize(train, forward)

