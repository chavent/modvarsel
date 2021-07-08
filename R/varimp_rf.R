######################
# Random Forest
######################
varimp_rf <- function(X, Y, nrep=10,ntree=300	,parallel=FALSE,myCluster=parallel::makeCluster(parallel::detectCores())){
  Y <- as.vector(Y)
  # RF on the initial dataset
  rf <- randomForest::randomForest(x=X,y=Y,ntree=ntree)
  Ypred <- rf$predicted #predictions OOB
  base_mse <- mean((Y-Ypred)^2)

  # Repetitions of VI measures for each variable
  p <- ncol(X)
  n <- nrow(X)
  mat_mse <- matrix(0,nrow=nrep,ncol=p)
  colnames(mat_mse) <- colnames(X)
  if (!parallel){
  for (r in 1:nrep) {
    for (j in 1:p){
      Xperm <- X
      Xperm[,j] <- Xperm[sample(1:n),j]
      rf <- randomForest::randomForest(x = Xperm, y = Y, ntree=ntree)
      Ypred <- rf$predicted #predictions OOB
      mat_mse[r,j] <-  mean((Y-Ypred)^2)
    }
  }
  }
  else if (parallel){
	      doParallel::registerDoParallel(myCluster)
  for (j in 1:p){
    mat_mse[,j]<-foreach::foreach(r =c(1:nrep), .combine = 'c') %dopar% {
      Xperm <- X
      Xperm[,j] <- Xperm[sample(1:n),j]
      rf <- randomForest::randomForest(x = Xperm, y = Y, ntree=ntree)
      Ypred <- rf$predicted #predictions OOB
      mean((Y-Ypred)^2)
      }
  }  
  parallel::stopCluster(myCluster)
  }
  list(mat_mse=mat_mse,base_mse=base_mse)
}
