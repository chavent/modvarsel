######################
# Random Forest
######################
varimp_rf <- function(X, Y, nrep=10){
  Y <- as.vector(Y)
  ntree <- 300
  # RF on the initial dataset
  rf <- randomForest::randomForest(x=X,y=Y,ntree=ntree)
  Ypred <- rf$predicted #predictions OOB
  base_mse <- mean((Y-Ypred)^2)

  # Repetitions of VI measures for each variable
  p <- ncol(X)
  n <- nrow(X)
  mat_mse <- matrix(0,nrow=nrep,ncol=p)
  colnames(mat_mse) <- colnames(X)

  for (r in 1:nrep) {
    for (j in 1:p){
      Xperm <- X
      Xperm[,j] <- Xperm[sample(1:n),j]
      rf <- randomForest::randomForest(x = Xperm, y = Y, ntree=ntree)
      Ypred <- rf$predicted #predictions OOB
      mat_mse[r,j] <-  mean((Y-Ypred)^2)
    }
  }

  list(mat_mse=mat_mse,base_mse=base_mse)
}
