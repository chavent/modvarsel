#' Variables importance
#'
#' This function  computes for a given method (\code{"linreg"}, \code{"sir"},
#' \code{"rf"},  \code{"pcr"},  \code{"plsr"},  \code{"ridge"}) the  importance
#' of the input variables. The  importance of a
#' variable (VI) is calculated by randomly permuting its values and calculating
#'  the predicted mean square error (MSE) of the model estimated with the
#'  perturbed data. Several permutations will give a several VI values for the
#'  same variable and then a more suitable idea its importance. The VI values
#'   can be compared to the so called basic VI value defined as the MSE of the
#'    model estimated with the non perturbed data.
#'
#' @details The following methods are available: multiple linear regression (\code{linreg}),
#' sliced inverse regression associated with kernel regression (\code{sir}),
#' random forests (\code{rf}), principal components regression  (\code{pcr}),
#' partial least squares regression (\code{plsr}), ridge regression (\code{ridge}).
#' If necessary the parameters are tuned as explained in \code{\link{choicemod}}.
#'
#' If the variable has an effect on the response variable, the random
#' permutation of its values will affect the estimation and its measure
#' of importance will take a high value.
#'
#' @param X the input numerical matrix of dimension nxp
#' @param Y the output numerical vector of size n
#' @param method a method chosen among \code{"linreg"}, \code{"sir"},
#' \code{"rf"}, \code{"pcr"},  \code{"plsr"} or  \code{"ridge"}
#' @param nperm the number of ramdom permutations of the values of the input
#' variables.
#'@return An object with S3 class "varimportance" and with the following components:
#' \item{mat_imp}{ a matrix of dimension \code{nperm} times \code{p}. Each colum  gives the  VI values of given variable.}
#'
#'\item{base_imp}{basic variable importance value i.e. the MSE with the non pertubed data.}
#'
#'@seealso  \code{\link{plot.varimportance}}, \code{\link{select.varimportance}}, \code{\link{choicemod}}
#'
#'@examples
#'data("simus")
#'X <- simus$X
#'Y <- simus$Y1
#'imp <- varimportance(X, Y, method = "linreg", nperm=15)
#'imp$mat_imp
#'apply(imp$mat_imp,2,mean)
#' @export



varimportance <- function(X, Y, method="linreg", nperm=10){

  if (!(method %in% c("linreg", "sir", "rf", "pcr", "plsr", "ridge")))
    stop("the \"method\" must be linreg, sir of rf",
         call. = FALSE)

  X <- as.matrix(X)
  n <- length(Y)
  if (method == "linreg"){
    res <- varimp_linreg(X, Y, nperm)
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
