#' @export
boxplot <- function(res,...) {
   UseMethod("boxplot")
 }


#' Boxplots for models comparison
#'
#' Parallel boxplots of the results (mean square errors or R2 coefficients)  obtained for complete models (with all the input variables) and for reduced models (with a selection of relevant variables) estimated by the function \code{choicemod}.
#'
#' @param res an object of class \code{choicemod}.
#' @param method if NULL, all the methods in \code{res$method} are plotted. Otherwise a subset of these methods (among \code{"linreg"}, \code{"sir"}, \code{"rf"}).
#' @param measure the criterion to be plotted. This should be \code{"mse"} (by default) or \code{"r2"}.
#' @param type this should be \code{"reduced"} (boxplot for reduced estimated models),  \code{"complete"} (boxplot for complete estimated models) or \code{"both"}. By default,  \code{type="reduced"}.
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
#' boxplot(res,method=c("linreg","sir"), measure="r2", col=rgb(0,1,0,0.2))
#'
#'@export

boxplot.choicemod <- function(res, method=NULL, measure = "mse",
  type = "reduced",  ...) {

  if (is.null(method)) method <- res$method
  if (!(all(method %in% res$method)))
    stop(paste("The argument \"method\" must be a
      subset of:", res$method, sep = " "),
      call. = FALSE)
  if (!(measure %in% c("mse", "r2")))
    stop("The argument \"measure\" must be either
      \"mse\" or \"r2\"", call. = FALSE)
  if (!(type %in% c("reduced", "complete", "both")))
    stop("The argument \"type\" must be \"reduced\"
      or \"complete\" or \"both\" ",
      call. = FALSE)

  colsel <- match(method,res$method)

  if (measure == "mse") {
    ylab="MSE"
    if (type == "reduced")
      mat <- res$mse[, colsel, drop = FALSE]
    if (type == "complete")
      mat <- res$mse_all[, colsel, drop = FALSE]
    if (type == "both") {
      mat <- data.frame(res$mse[,colsel,drop = FALSE],
        res$mse_all[, colsel, drop = FALSE])
    }
  }

  if (measure == "r2") {
    ylab="R2"
    if (type == "reduced")
      mat <- res$r2[, colsel, drop = FALSE]
    if (type == "complete")
      mat <- res$r2_all[, colsel, drop = FALSE]
    if (type == "both") {
      mat <- data.frame(res$r2[, colsel, drop = FALSE],
        res$r2_all[, colsel, drop = FALSE])
    }
  }

  graphics::boxplot(mat,ylab = ylab, ...)
}
