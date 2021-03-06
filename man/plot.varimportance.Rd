% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.varimportance.R
\name{plot.varimportance}
\alias{plot.varimportance}
\title{Plots of the importance of the covariates}
\usage{
\method{plot}{varimportance}(x, choice = "boxplot", order = FALSE,
  xlab = NULL, ylab = NULL, cutoff = FALSE, ...)
}
\arguments{
\item{x}{an object of class \code{varimportance}.}

\item{choice}{if "boxplot" the parallel boxplots of the VI is provided.
If "meanplot" the plot of the mean VI is provided.}

\item{order}{if TRUE, the mean VI values are plotted by decreasing order.}

\item{xlab}{X axis label.}

\item{ylab}{Y axis label.}

\item{cutoff}{if TRUE a vertical green line is plotted to give a
single change point position (only when \code{order=TRUE})}

\item{\dots}{further arguments passed to or from other methods.}
}
\description{
This funtion provides two graphical representation:
the parallel boxplots of the variable importance (VI)  of the covariates and
the plot of the mean VI of the covariates.
}
\details{
The original MSE is indicated by a red horizontal line.
}
\examples{
data("simus")
X <- simus$X
Y <- simus$Y1
imp <- varimportance(X,Y,method="linreg",nperm=8)
plot(imp,choice="boxplot", col="lightgreen")
plot(imp,choice="meanplot")
plot(imp,choice="meanplot", order = TRUE, cutoff = TRUE)

}
\seealso{
\code{\link{varimportance}}, \code{\link{select.varimportance}}
}
