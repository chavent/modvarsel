#' @export
barplot <- function(res,...) {
  UseMethod("barplot")
}


#' @title Barplots of the selected variables
#'
#' @description Barplots with informations about the variables selected in the reduced models estimated by the function \code{choicemod}.
#'
#' @param res an object of class \code{choicemod}.
#' @param method one of the methods in \code{res$method} (among \code{"linreg"}, \code{"sir"}, \code{"rf"},
#' \code{"pcr"}, \code{"plsr"}, \code{"ridge"}).
#' @param type this should be \code{"varsel"} (proportion of selection each variable in the reduced model).
#' or \code{"modsize"} (frequency of the size of each reduced model).
#' @param \dots further arguments passed to or from other methods.
#'
#' @seealso \code{\link{choicemod}}, \code{\link{boxplot.choicemod}}
#'
#'@examples
#' data("simus")
#' res <- simus$res1
#' class(res)
#' res$method
#'barplot(res, method="sir", type="varsel", las = 2,  main = "sir", col = rgb(1,1,0,0.2))
#' barplot(res, method="sir", type="sizemod", las = 2, main = "sir", col = rgb(0,1,1,0.2))
#'
#' @export



barplot.choicemod <- function(res, method="linreg",
  type="varsel",...) {

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

  imeth <- match(method, res$method)
  if (type == "varsel") {
    prop <- res$pvarsel[,imeth]
    graphics::barplot(prop[order(prop, decreasing = TRUE)],
            ylab="percent",...)
  }
  if (type == "sizemod") {
    p <- nrow(res$pvarsel)
    tb <- table(res$sizemod[,imeth])
    tbsize <- rep(0,p)
    names(tbsize) <- 1:p
    tbsize[as.numeric(names(tb))] <- tb
    #graphics::barplot(tbsize,ylab="frequency", xlab="size of the reduced model",...)
    graphics::barplot(tbsize/nrow(res$sizemod),ylab="percent", xlab="size of the reduced model",...)
  }
}
