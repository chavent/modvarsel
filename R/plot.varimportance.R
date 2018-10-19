#' Plots of the importance of the covariates
#'
#' This funtion provides two graphical representation:
#' the parallel boxplots of the variable importance (VI)  of the covariates and
#' the plot of the mean VI of the covariates.
#'
#'
#' @param x an object of class \code{varimportance}.
#' @param choice if "boxplot" the parallel boxplots of the VI is provided.
#' If "meanplot" the plot of the mean VI is provided.
#' @param order if TRUE, the mean VI values are plotted by decreasing order.
#' @param ylab Y axis label.
#' @param xlab X axis label.
#' @param cutoff if TRUE a vertical green line is plotted to give a
#' single change point position (only when \code{order=TRUE})
#' @param \dots further arguments passed to or from other methods.
#'
#'@details The original MSE is indicated by a red horizontal line.
#'
#' @seealso \code{\link{varimportance}}, \code{\link{select.varimportance}}
#'
#' @examples
#' data("simus")
#' X <- simus$X
#' Y <- simus$Y1
#' imp <- varimportance(X,Y,method="linreg",nperm=8)
#' plot(imp,choice="boxplot", col="lightgreen")
#' plot(imp,choice="meanplot")
#' plot(imp,choice="meanplot", order = TRUE, cutoff = TRUE)
#'
#' @export

plot.varimportance <- function(x, choice = "boxplot",
                          order = FALSE, xlab = NULL, ylab = NULL, cutoff = FALSE, ...){

  if (!(choice %in% c("boxplot", "meanplot")))
    stop("\"choice\" must be either \"boxplot\" or \"meanplot\"",call. = FALSE)
  if (!(order %in% c(TRUE, FALSE)))
    stop("\"order\" must be either TRUE or FALSE",call. = FALSE)

  names_var <- colnames(x$mat_imp)
  if (is.null(ylab))
    ylab="importance"
  if (choice == "boxplot") {
    graphics::boxplot(x$mat_imp, ylab = ylab,
            names = names_var, las = 2, xlab = xlab, ...)
    graphics::abline(h = x$base_imp, col = 2,lwd = 2)
  }
  if (choice == "meanplot"){
    p <- ncol(x$mat_imp)
    imp_mean <- apply(x$mat_imp,2,mean)
    if (is.null(xlab))
      xlab=" "
    if (order == FALSE) {
      graphics::plot(1:p, imp_mean, pch = 16, xlab = xlab,
           ylab =ylab, axes=FALSE, ...)
      graphics::lines(imp_mean, lty = 2)
      graphics::axis(1, at = 1:p, labels = names_var,
           col.axis = "black", las = 2, lty = 0)
      graphics::axis(2, col.axis = "black")
      graphics::abline(h = x$base_imp, col = 2, lwd = 2)
    } else {
      mean_sorted <- sort(imp_mean,decreasing = TRUE)
      seg <- changepoint::cpt.meanvar(mean_sorted)
      nsel <- changepoint::cpts(seg)
      graphics::plot(1:p, mean_sorted, pch = 16, xlab = xlab,
           ylab=ylab, axes = FALSE, ...)
      graphics::lines(1:p, mean_sorted, lty = 3)
      graphics::axis(1, at = 1:p,labels = names(mean_sorted),
           col.axis = "black", las = 2,lty = 0)
      graphics::axis(2, col.axis = "black")
      if (cutoff==TRUE)
        graphics::abline(v = nsel+0.5,col=3,lwd = 2)
      graphics::abline(h = x$base_imp, col = 2,lwd = 2)
    }
  }
}


