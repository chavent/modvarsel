######################
# Linear regression
######################
varimp_linreg <- function(X, Y, nrep=10,parallel=FALSE,numCores=parallel::detectCores()){

  base_mse <- mean((stats::lm(Y~as.matrix(X))$residuals)^2)

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
      res <- stats::lm(Y~as.matrix(Xperm))
      mat_mse[r,j] <-  mean(res$residuals^2)
      #res <- glmnet::glmnet(Xperm, Y, family = "gaussian",
       #                     alpha=0, lambda = cvfit$lambda.min,
       #      standardize = FALSE)

      #mat_mse[r,j] <-  mean((Y-predict(res,Xperm))^2)
    }
  }
  }
  else if (parallel){
  doParallel::registerDoParallel(numCores)
  for (j in 1:p){
    mat_mse[,j]<-foreach::foreach(r =c(1:nrep), .combine = 'c') %dopar% {
      Xperm <- X
      Xperm[,j] <- Xperm[sample(1:n),j]
      res <- stats::lm(Y~as.matrix(Xperm))
      mean(res$residuals^2)
      }
  }  
  }
  list(mat_mse=mat_mse,base_mse=base_mse)
}
