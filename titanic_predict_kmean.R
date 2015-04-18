source("titanic_predict.R")
library("RSNNS")

fwd <- function(model, data) {
  ret=predict(model,data)
  return(ret)
}

errors <- c()

trials = 2:9
for (i in trials) {

  train <- function(data, target) {
    model <- rbf(data, target,
                 size=i,
                 maxit=250,
                 #initFuncParams=c(0, 1, 0, 0.01, 0.01),
                 #learnFuncParams=c(1e-8, 0, 1e-8, 0.1, 0.8), linOut=TRUE
                 )
    
    return (model)
  }
  
  error <- analize(train, fwd)
  errors <- rbind(errors, error)

}

plot(c(min(trials),max(trials)), c(min(errors), max(errors)) ,type="n")
lines(trials, errors[,1], col = "black")
lines(trials, errors[,2], col="red")
 
