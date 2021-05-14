#' function to calculate mean precipitation over x time period
#' @param i starting index position in `prec`
#' @param period number of days over which to calculate mean
#' @param id number of precipitation values per day
#' @param prec precipitation values
#' @return
#' @noRd
mprec <- function(i, period, id, prec) {
  prec_mod <- c(prec, prec)
  prec_mean <- mean(prec_mod[i: (i + (period * id) - 1)], na.rm = TRUE)
  return(prec_mean)
}
