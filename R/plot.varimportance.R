#' Plots of the importance of the variables
#'
#' This function provides the parallel boxplots of the importance of the variables (VI) and the plot of the mean importance of the variables. The baseline value of importance is indicated by a red horizontal line for comparison purpose. If \code{cutoff=TRUE} the variables are ordered by decreasing mean importance and a single change point position (in mean and variance) is indicated by a green vertical line.
#'
#' @param x an object of class \code{varimportance}.
#' @param choice this should be "boxplot" (for parallel boxplots of the importance of the variables), "meanplot" (for the plot of the mean importance of the variables).
#' @param cutoff if TRUE, the variables are ordered by decreasing mean importance and a single change point position (in mean and variance) is indicated by a green vertical line.
#' @param \dots further arguments passed to or from other methods.
#'
#' @seealso \code{\link{varimportance}}, \code{\link{select.varimportance}}
#'
#' @examples
#' data("simus")
#' X <- simus$X
#' Y <- simus$Y1
#' imp <- varimportance(X,Y,method="linreg",nperm=8)
#' plot(imp,choice="boxplot", col="lightgreen") #by default
#' plot(imp,choice="meanplot", cutoff = FALSE)
#' plot(imp,choice="meanplot", cutoff = TRUE)
#'
#' @export

plot.varimportance <- function(x, choice = "boxplot",
                          cutoff = FALSE, ...){

  if (!(choice %in% c("boxplot", "meanplot")))
    stop("\"choice\" must be either \"boxplot\" or \"meanplot\"",call. = FALSE)
  if (!(cutoff %in% c(TRUE, FALSE)))
    stop("\"cutoff\" must be either TRUE or FALSE",call. = FALSE)

  names_var <- colnames(x$mat_imp)

  if (choice == "boxplot") {
    graphics::boxplot(x$mat_imp, xlab = " ", ylab = "importance",
            names = names_var, las = 2, ...)
    graphics::abline(h = x$base_imp, col = 2,lwd = 2)
  }
  if (choice == "meanplot"){
    p <- ncol(x$mat_imp)
    imp_mean <- apply(x$mat_imp,2,mean)
    if (cutoff == FALSE) {
      graphics::plot(1:p, imp_mean, pch = 16, xlab = " ",
           ylab = "mean of importance", axes=FALSE, ...)
      graphics::lines(imp_mean, lty = 2)
      graphics::axis(1, at = 1:p, labels = names_var,
           col.axis = "black", las = 2, lty = 0)
      graphics::axis(2, col.axis = "black")
      graphics::abline(h = x$base_imp, col = 2, lwd = 2)
    } else {
      mean_sorted <- sort(imp_mean,decreasing = TRUE)
      seg <- changepoint::cpt.meanvar(mean_sorted)
      nsel <- changepoint::cpts(seg)
      graphics::plot(1:p, mean_sorted, pch = 16, xlab = " ",
           ylab= "mean of importance", axes = FALSE, ...)
      graphics::lines(1:p, mean_sorted, lty = 3)
      graphics::axis(1, at = 1:p,labels = names(mean_sorted),
           col.axis = "black", las = 2,lty = 0)
      graphics::axis(2, col.axis = "black")
      graphics::abline(v = nsel+0.5,col=3,lwd = 2)
      graphics::abline(h = x$base_imp, col = 2,lwd = 2)
    }
  }
}


