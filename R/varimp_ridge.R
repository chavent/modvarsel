varimp_ridge <- function(X, Y, nrep=10){
  X <- as.matrix(X)

  #ridge linear regression on the initial dataset
  cvfit <- glmnet::cv.glmnet(X, Y, family = "gaussian", alpha=0,
             standardize = FALSE, nfolds = 10, grouped=FALSE)
  Ypred <- stats::predict(cvfit,X, s = "lambda.min")
  base_mse <- mean((Y-Ypred)^2)

  # Repetitions of VI measures for each variable
  p <- ncol(X)
  n <- nrow(X)
  mat_mse <- matrix(0,nrow = nrep,ncol = p)
  colnames(mat_mse) <- colnames(X)

  for (r in 1:nrep) {
    cat("Replication ", r," on ", nrep, fill = TRUE)
    for (j in 1:p){
      Xperm <- X
      Xperm[,j] <- Xperm[sample(1:n),j]

      cvfit <- glmnet::cv.glmnet(Xperm, Y, family = "gaussian", alpha=0,
             standardize = FALSE, nfolds = 10, grouped=FALSE)
      Ypred <- stats::predict(cvfit,X, s = "lambda.min")
      mat_mse[r,j] <- mean((Y-Ypred)^2)

    }
  }
  list(mat_mse = mat_mse,base_mse = base_mse)
}
