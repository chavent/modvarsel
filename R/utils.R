#' Change point detection
#'
#' Detects a single change point position (in mean and variance) in a vector of numerical values.
#'
#' @param x a vector.
#' @return the position of the change point.
#' @export

find.cpt <- function(x) {
  x <- as.vector(x)
  if (length(x) >= 4) {
    cpt <- changepoint::cpt.meanvar(x,
      class=TRUE)@cpts[1]
  } else {
    if (length(x)==1)
      cpt=1 else
        cpt <- changepoint::cpt.mean(x,
          class=TRUE)@cpts[1]
  }
  cpt
}

#' Simulated datasets
#'
#' Simulated data generated from two fictitious model: a parametric linear model
#' (M1) and a semiparametric model (M2). The vector of parameters associated with each
#' covariate (in both models) gives only the five first covariates linked with
#'the response variable.
#'
#'@format A list with the following elements:
#' \describe{
#'   \item{X}{a numerical matrix with n=200 realisations of the p=15 covariates
#'   generated independantly with uniform distributions.}
#'   \item{Y1}{the response vector of size 200 obtained from X with the model (M1)}
#'   \item{Y2}{the response vector of size 200 obtained from X with the model (M2)}
#'   \item{Xnew}{a numerical matrix of n=10 new observations (not used to build the response vectors)
#'   of the p=15 covariates}
#'   \item{res1}{results of the \code{choicemod} function with \code{Y1} as response variable (M1)}
#'   \item{res2}{results of the \code{choicemod} function with \code{Y2} as response variable (M2)}
#' }
#' @source \url{https://chavent.github.io/modvarsel/modvarsel-intro.html}
#' @name simus
NULL

#' Meat tenderness data
#'
#' This dataset describes 71 young bulls on 21 muscular biomarkers
#' and a numerical variable measuring meat tenderness. A short description of the variables is available here :
#'\url{https://chavent.github.io/modvarsel/modvarsel-intro.html}
#'@format A list with the following elements:
#' \describe{
#'   \item{dat}{a numerical matrix with 71 rows (one row for each young bull)
#'   and 22 columns (the 21 first columns are biomarkers, the last one
#'   measures meat tenderness.}
#'   \item{res}{results of the \code{choicemod} function for this dataset}
#'   \item{imp1}{results of the \code{varimportance} function with \code{linreg}}
#'   \item{imp2}{results of the \code{varimportance} function with \code{sir}}
#'   \item{imp3}{results of the \code{varimportance} function with \code{rf}}
#' }
#' @source These experimental data were obtained on animals coming from the EU FP6
#' Integrated Project ProSafeBeef (FOODCT-2006-36241).
#' @name psbst
NULL



