
#question : ne faudrait-il pas tuner la fenêtre en dehors de la boucle des réplications comme en RF ?
#tester que Y est bien un vecteur

varimp_sir <- function(X, Y, nrep=10){
  X <- as.matrix(X)
  #SIR on the initial dataset and tuning of the bandwidth
  beta <- edrGraphicalTools::edr(Y, X, H = 10, K = 1,
    method="SIR-I")$matEDR[,1,drop=FALSE]
  indice <- X%*%beta
  hopt <- cv_bandwidth(indice,Y,graph.CV=FALSE)$hopt
  mat <- cbind(Y,indice)
  matord <- mat[order(mat[,2]),]
  Yord <- matord[,1]
  indice_ord <- matord[,2]
  Yesti <- stats::ksmooth(indice, Y, kernel = "normal",
    bandwidth = hopt, x.points = indice_ord)$y
  base_mse <- mean((Yord-Yesti)^2)

  # Repetitions of VI measures for each variable
  p <- ncol(X)
  n <- nrow(X)
  mat_mse <- matrix(0,nrow=nrep,ncol=p)
  colnames(mat_mse) <- colnames(X)

  for (r in 1:nrep) {
    for (j in 1:p){
      Xperm <- X
      Xperm[,j] <- Xperm[sample(1:n),j]
      beta <- edrGraphicalTools::edr(Y,
        Xperm, H = 10, K = 1,
        method = "SIR-I")$matEDR[,1,drop=FALSE]
      indice <- Xperm%*%beta
      #hopt <- cv_bandwidth(indice,Y,graph.CV=FALSE)$hopt
      mat <- cbind(Y,indice)
      matord <- mat[order(mat[,2]),]
      Yord <- matord[,1]
      indice_ord <- matord[,2]
      Yesti <- stats::ksmooth(indice, Y, kernel = "normal",
        bandwidth = hopt,
        x.points = indice_ord)$y
      mat_mse[r,j] <- mean((Yord-Yesti)^2)
    }
  }

  list(mat_mse=mat_mse, base_mse  =base_mse)
}


#' Tune bandwidth
#'
#' \code{barplot} tunes by LOO cross-validation
#' the bandwidth for Kernel Regression Smoother.
#'
#'@param x a vector with the input values
#'@param y a vector with the output values
#'@param hmin the minimum bandwidth
#'@param hmax the maximum bandwidth
#'@param nbh size of the grid of bandwidth
#'@param graph.CV If TRUE plot the CV MSE against bandwidth values
#'@export
#'
cv_bandwidth <- function(x, y, hmin=(max(x)-min(x))/20,
                hmax=(max(x)-min(x))/2, nbh=25,
                graph.CV=TRUE){
  n <- length(x)
  vecth <- seq(from = hmin, to = hmax, length = nbh)
  matCV <- cbind(vecth, rep(0,nbh))
  for (h in 1:nbh){
    ypred <- rep(0, n)
    for (i in 1:n){
      ypred[i]<- stats::ksmooth(x[-i], y[-i], kernel = "normal",
                  band=vecth[h], x.points=x[i])$y
    }
    matCV[h,2]<-sum((y-ypred)^2)
  }
  hopt <- matCV[which(matCV[,2] == min(matCV[,2], na.rm=TRUE)),1]
  if (graph.CV==TRUE){
    graphics::plot(matCV, xlab="bandwidth", ylab="CV MSE", pch=" ", type="l")
    graphics::abline(v = hopt, col = 2)
  }
  list(matCV = matCV, hopt = hopt)
}

