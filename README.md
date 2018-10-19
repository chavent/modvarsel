# modvarsel

## Description
This R package implements a methodology to choose among several regression methods the best one to predict a numerical
response variable Y and to select simultaneously the most interesting covariates strongly linked to Y. A train/test samples approach is used to determine which regression method (including or not covariates selection) is the most accurate for a given regression dataset. The following regression methods are available: multiple linear regression, SIR regression associated with kernel estimation of the link function, Random regression Forests, principal components regression, partial least squares regression and ridge regression.
The procedure of variables selection implements the covariate permutation technique to measure the importance of each covariate (VI) for any of the previous regression method. 

More details are available is the following vignette:

https://chavent.github.io/modvarsel/modvarsel-intro.html

## Install

To install the current development version from github, use :

```{r eval=FALSE}
devtools::install_github("chavent/modvarsel")
# This needs the devtools package to be installed :
# install.packages("devtools")
```
