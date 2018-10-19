#' Generic parallel boxplots
#'
#' \code{boxplot} is a generic function for parallel boxplots.
#' @param res an object with the values to plot.
#' @param ... additional arguments.
#' @export
boxplot <- function(res,...) {
   UseMethod("boxplot")
 }


#' Parrallel boxplots of choicemod
#'
#' The parallel boxplots are built with the MSE values obtained at each replication
#' of the procedure implemented in \link{choicemod}. At each replication, each regression
#' method estimates both the reduced model (with covariates selection)
#' and the complete models (without covariates selection). The boxplots represent the
#' test mean square errors (performed on test samples) of each regression method including (or not) covariates selection.
#'
#' @param res an object of class \code{choicemod}.
#' @param method if NULL, all the methods in \code{res$method} are plotted.
#' Otherwise a subset of these methods (\code{"linreg"}, \code{"sir"}, \code{"rf"}, \code{"pcr"},
#' \code{"plsr"}, \code{"ridge"}).
#' @param type this should be \code{"reduced"} (boxplot for reduced estimated models),
#'  \code{"complete"} (boxplot for complete estimated models) or \code{"both"}.
#'   By default,  \code{type="reduced"}.
#' @param ylab Y axis label.
#' @param \dots further arguments passed to or from other methods.
#'
#' @seealso \code{\link{choicemod}}, \code{\link{barplot.choicemod}}
#'
#' @examples
#' data("simus")
#' res <- simus$res1
#' class(res)
#' res$method
#' boxplot(res, type="reduced", col="gold", main="Models with variables selection, N=10")
#' boxplot(res,type="complete",col=rgb(0,0,1,0.2), main="Models with all the variables, N=10")
#' boxplot(res,type="both", col = "aquamarine", las = 2, main = "N = 10 replications")
#'
#'@export

boxplot.choicemod <- function(res, method=NULL,
  type = "reduced", ylab=NULL, ...) {

  if (is.null(method)) method <- res$method
  if (!(all(method %in% res$method)))
    stop(paste("The argument \"method\" must be a
      subset of:", res$method, sep = " "),
      call. = FALSE)
  if (!(type %in% c("reduced", "complete", "both")))
    stop("The argument \"type\" must be \"reduced\"
      or \"complete\" or \"both\" ",
      call. = FALSE)

  colsel <- match(method,res$method)
  if (is.null(ylab))
    ylab="MSE"
  if (type == "reduced")
    mat <- res$mse[, colsel, drop = FALSE]
  if (type == "complete")
    mat <- res$mse_all[, colsel, drop = FALSE]
  if (type == "both") {
    mat <- data.frame(res$mse[,colsel,drop = FALSE],
                      res$mse_all[, colsel, drop = FALSE])
  }
  graphics::boxplot(mat,ylab = ylab, ...)
}
