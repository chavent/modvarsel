######################
# PCR
######################
varimp_pcr <- function(X, Y, nrep=10){
  n <- nrow(X)
  model <- pls::pcr(Y~., data = data.frame(X),
    validation = "LOO", scale=FALSE)
  # rmsep <- pls::RMSEP(model, intercept = FALSE)$val["CV",,]
  rmsep <- sqrt(model$validation$PRESS/n)
  ncomp <- find.cpt(rmsep)

  Ypred <- as.vector(stats::predict(model, data.frame(X),
    ncomp = ncomp))
  base_mse <- mean((Y - Ypred)^2)

  # Repetitions of VI measures for each variable
  p <- ncol(X)
  n <- nrow(X)
  mat_mse <- matrix(0,nrow = nrep,ncol = p)
  colnames(mat_mse) <- colnames(X)

  for (r in 1:nrep) {
    for (j in 1:p){
      Xperm <- X
      Xperm[,j] <- Xperm[sample(1:n),j]

      model <- pls::pcr(Y~., data = data.frame(Xperm),
        validation = "CV", scale=FALSE)
      # rmsep <- pls::RMSEP(model, intercept = FALSE)$val["CV",,]
      rmsep <- sqrt(model$validation$PRESS/n)
      ncomp <- find.cpt(rmsep)
      Ypred <- as.vector(stats::predict(model, data.frame(Xperm),
        ncomp=ncomp))
      mat_mse[r,j] <- mean((Y - Ypred)^2)
    }
  }
  list(mat_mse = mat_mse,base_mse = base_mse)
}
