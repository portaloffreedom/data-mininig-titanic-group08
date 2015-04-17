source("titanic_predict.R")
library("RSNNS")

train <- function(data, target) {
  model <- rbf(data, target,
               size=3,
               #maxit=1000,
               #initFuncParams=c(0, 1, 0, 0.01, 0.01),
               #learnFuncParams=c(1e-8, 0, 1e-8, 0.1, 0.8), linOut=TRUE
               )
  
  return (model)
}

fwd <- function(model, data) {
  ret=predict(model,data)
  return(ret)
}

error = analize(train, fwd)
