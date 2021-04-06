#' Variables importance
#'
#' This function  computes for a given regression method
#' (\code{"linreg"}, \code{"pcr"},  \code{"plsr"},  \code{"ridge"},\code{"sir"},
#' \code{"rf"},  ) the  importance
#' of the covariates by estimating the response variable with some perturbations
#' of the covariates and computing the error due to these perturbations.
#' The  variable importance (VI) of a covariate is  the mean square error (MSE) when
#'  the values of this variable are randomly permuted. Covariates with higher VI are then more
#'  important to predict the response variable. This procedure is replicated several times
#'  giving several values of VI for each covariate.  The original MSE (with no perturbation of the data)
#'  is also performed for comparison purpose.
#'
#'
#' @param X a numerical matrix containing the \code{p} variables in the model.
#' @param Y a numerical response vector.
#' @param method a regression method
#' (\code{"linreg"}, \code{"sir"}, \code{"rf"}, \code{"pcr"}, \code{"plsr"}, \code{"ridge"}).
#' @param nperm the number of random perturbations to perform the importance of the covariates (VI).
#'
#'@return An object with S3 class "varimportance" and the following components:
#' \item{mat_imp}{a matrix of dimension \code{nperm} times \code{p}
#' with the VI values for each permuation (in rows) and each covariate (un column).}
#' \item{base_imp}{the original mean square error.}
#'
#'@seealso  \code{\link{plot.varimportance}}, \code{\link{select.varimportance}}, \code{\link{choicemod}}
#'
#'@examples
#'data("simus")
#'X <- simus$X
#'Y <- simus$Y1
#'imp <- varimportance(X, Y, method = "linreg", nperm=15)
#'plot(imp)
#'imp$mat_imp
#'apply(imp$mat_imp,2,mean)
#' @export



varimportance <- function(X, Y, method="linreg", nperm=10,parallel=FALSE,numCores=parallel::detectCores()){

  if (!(method %in% c("linreg", "sir", "rf", "pcr", "plsr", "ridge")))
    stop("the \"method\" must be linreg, sir of rf",
         call. = FALSE)

  X <- as.matrix(X)
  n <- length(Y)
  if (method == "linreg"){
    res <- varimp_linreg(X, Y, nperm,parallel=parallel,numCores=numCores))
  }
  if (method == "sir"){
    res <- varimp_sir(X, Y, nperm)
  }
  if (method == "rf"){
    res <- varimp_rf(X, Y, nperm)
  }
  if (method == "pcr"){
    res <- varimp_pcr(X, Y, nperm)
  }
  if (method == "plsr"){
    res <- varimp_plsr(X, Y, nperm)
  }
  if (method == "ridge"){
    res <- varimp_ridge(X, Y, nperm)
  }
  #mat_r2 <- 1-res$mat_mse/as.numeric(var(Y)*(n-1)/n)
  #base_r2 <- 1- res$base_mse/as.numeric(var(Y)*(n-1)/n)
  structure(
  list(mat_imp = res$mat_mse, base_imp = res$base_mse,
    method = method),
  class = "varimportance"
  )
}
