"totPercents2.HH" <-
function(tab, digits=1) {
  dim <- length(dim(tab))
  if (dim != 2) stop("totPercents2.HH is defined only for two-way tables.")
  round(100 * tab / sum(tab), digits)
}

