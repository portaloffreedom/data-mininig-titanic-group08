source("titanic_predict.R")
library("RSNNS")

fwd <- function(model, data) {
  ret=predict(model,data)
  return(ret)
}

errors <- c()

for (i in 2:6) {

  train <- function(data, target) {
    model <- rbf(data, target,
                 size=i,
                 maxit=300,
                 #initFuncParams=c(0, 1, 0, 0.01, 0.01),
                 #learnFuncParams=c(1e-8, 0, 1e-8, 0.1, 0.8), linOut=TRUE
                 )
    
    return (model)
  }
  
  error <- analize(train, fwd)
  errors <- rbind(errors, error)

}
 