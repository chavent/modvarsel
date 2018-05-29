#' @export
#'
select <- function(imp,...) {
  UseMethod("select")
}

#' Automatic selection of variables
#'
#' Selection of the p' < p variables with the highest measure of importance (VI). The number p' is either defined by detecting a single change point position (in mean and variance) in the ordered VI values or by fixing a priori the number p' of selected variables.
#'
#' @param imp an object of class \code{varimportance}.
#' @param cutoff if TRUE a change point criterion is used to define a cutoff value and select variables with mean VI above this cutoff.
#' @param nbsel if \code{cutoff=FALSE} the number of variables to be selected.
#' @param \dots further arguments passed to or from other methods.
#'
#' @return an object of class \code{list} with two components.
#' \item{var}{a vector with the names of the selected variables}
#' \item{indices}{a vector with the indices of the selected variables}
#'
#' @seealso \code{\link{varimportance}}, \code{\link{plot.varimportance}}
#'
#' @examples
#' data("simus")
#' X <- simus$X
#' Y <- simus$Y1
#' imp <- varimportance(X,Y,method="linreg",nperm=8)
#' select(imp, cutoff=TRUE) # by default
#' select(imp, cutoff=FALSE, nbsel=7)
#'
#' @export
#'
#===========================================
# Automatic selection of variables
#===========================================
select.varimportance <- function(imp, cutoff = TRUE, nbsel = NULL, ...) {

  if (!(cutoff %in% c(TRUE, FALSE)))
    stop("\"cutoff\" must be \"TRUE\" or \"FALSE\"",
      call. = FALSE)
  if ((cutoff == FALSE) && (is.null(nbsel)))
     stop("If \"cutoff=FALSE\" the value \"nbsel\"
       must be given", call. = FALSE)
  if (!is.null(nbsel))
    if ( !(nbsel>0) ||  !(nbsel <= ncol(imp$mat_imp)) ||
         !(all.equal(nbsel, as.integer(nbsel))))
      stop("\"nbsel\" must be a positive integer
        between 1 and p", call. = FALSE)

  p <- ncol(imp$mat_imp)
  imp_mean <- apply(imp$mat_imp,2,mean)
  mean_sorted <- sort(imp_mean,decreasing=TRUE)

  if (cutoff == TRUE) {
    nbsel_auto <- find.cpt(mean_sorted)
    var_select <- names(mean_sorted)[1:nbsel_auto]
    indices <- order(imp_mean,decreasing=TRUE)[1:nbsel_auto]
  } else {
    var_select <- names(mean_sorted)[1:nbsel]
    indices <- order(imp_mean,decreasing=TRUE)[1:nbsel]
  }
  list(var=var_select, indices=indices)
}

