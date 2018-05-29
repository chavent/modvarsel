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
