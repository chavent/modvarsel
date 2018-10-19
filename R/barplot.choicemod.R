#' Generic barplot
#'
#' \code{barplot} is a generic function for barplot.
#' @param res an object with the values to plot.
#' @param ... additional arguments.
#' @export
barplot <- function(res,...) {
  UseMethod("barplot")
}


#' @title Barplots of choicemode
#'
#' @description This function gives two types of barplots for a given regression method: the barplot of the occurences (in percent)
#' of selection of each covariates  and the barplot
#' of the occurences (in percent) of the number of selected covariates (size of the reduced model).
#'
#' @param res an object of class \code{choicemod}.
#' @param method a regression methods in \code{res$method} ( \code{"linreg"}, \code{"sir"}, \code{"rf"}, \code{"pcr"},
#' \code{"plsr"}, \code{"ridge"}).
#' @param type if \code{"varsel"} the occurences (in percent)
#' of selection of each covariates are plotted.
#' If \code{"modsize"} the occurences (in percent) of
#' the sizes of the reduced models are plotted.
#' @param ylab Y axis label ("percent" by default).
#' @param xlab X axis label  ("size of the reduced model" by default
#'  when when \code{type="modsize"}).
#' @param \dots further arguments passed to or from other methods.
#'
#' @seealso \code{\link{choicemod}}, \code{\link{boxplot.choicemod}}
#'
#'@examples
#' data("simus")
#' res <- simus$res1
#' class(res)
#' res$method
#' barplot(res, method="sir", type="varsel", las = 2,  main = "sir", col = rgb(1,1,0,0.2))
#' barplot(res, method="sir", type="sizemod", las = 2, main = "sir", col = rgb(0,1,1,0.2))
#'
#' @export



barplot.choicemod <- function(res, method="linreg",
  type="varsel", xlab=NULL, ylab=NULL, ...) {

  if (length(method) != 1)
      stop("The argument \"method\" is not correct",
           call. = FALSE)
  if (!(method %in% res$method))
    stop(paste("The argument \"method\" must be
      one of the following:", res$method,
      sep = " "), call. = FALSE)
  if (!(type %in% c("varsel","sizemod")))
    stop("The argument \"type\" must be
      \"varsel\" or \"sizemod\"",
      call. = FALSE)
  if (is.null(ylab))
    ylab="percent"
  imeth <- match(method, res$method)
  if (type == "varsel") {
    prop <- res$pvarsel[,imeth]
    graphics::barplot(prop[order(prop, decreasing = TRUE)],
            ylab=ylab, xlab=xlab, ...)
  }
  if (type == "sizemod") {
    p <- nrow(res$pvarsel)
    tb <- table(res$sizemod[,imeth])
    tbsize <- rep(0,p)
    names(tbsize) <- 1:p
    tbsize[as.numeric(names(tb))] <- tb
    #graphics::barplot(tbsize,ylab="frequency", xlab="size of the reduced model",...)
    if (is.null(xlab))
      xlab="size of the reduced model"
    graphics::barplot(tbsize/nrow(res$sizemod),ylab=ylab, xlab=xlab,...)
  }
}
