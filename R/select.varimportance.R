#' Variables selection
#'
#' \code{select} is a generic function for selection of covariates
#'in a prediction model.
#' @param imp an object with the importance of variables.
#' @param ... additional arguments.
#' @export
select <- function(imp,...) {
  UseMethod("select")
}


#' Automatic selection of variables
#'
#' Chooses automaticaly the covariates relevant to predict the response variable.
#' By default a change point criterion is used to define a cutoff value
#'  and the covariates with mean VI above this cutoff value are selected.
#' It is also possible to choose the number of covariates to select rather than
#' using the changepoint cutoff value.
#'
#' @param imp an object of class \code{varimportance}.
#' @param cutoff if TRUE covariates above the changepoint cutoff value are selected.
#' @param nbsel  the number of covariates to select (only if \code{cutoff=FALSE}).
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
  if (!is.null(nbsel)) cutoff <- FALSE

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

