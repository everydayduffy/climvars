#' function to calculate the mean of climate values over x time period
#' @param i starting index position in climate value in `climval`
#' @param period number of days over which to calculate mean
#' @param id number of climate values per day
#' @param climval climate values
#' @return mean value of climval
#' @noRd
mean_climval <- function(i, period, id, climval) {
  climval_mod <- c(climval, climval)
  climval_mean <- mean(climval_mod[i: (i + (period * id) - 1)], na.rm = TRUE)
  return(climval_mean)
}

#' function to calculate the sum of climate values over x time period
#' @param i starting index position in climate value in `climval`
#' @param period number of days over which to calculate sum
#' @param id number of climate values per day
#' @param climval climate values
#' @return sum value of climval
#' @noRd
sum_climval <- function(i, period, id, climval) {
  climval_mod <- c(climval, climval)
  climval_sum <- sum(climval_mod[i: (i + (period * id) - 1)], na.rm = TRUE)
  return(climval_sum)
}

#' function to calculate the mean of climate values for a quarter of year
#' @param i starting index position in climate value in `climval`
#' @param int number of climate values per quarter (91 days)
#' @param climval vector of climate data
#' @return mean value of climval
#' @noRd
qtr <- function(i, int, climval) {
  dclim <- c(climval, climval)
  mclim <- mean(dclim[i: (i + int)], na.rm = TRUE)
  return(mclim)
}

#' function to calculate the sum of climate values for a quarter of year
#' @param i starting index position in climate value in `climval`
#' @param int number of climate values per quarter (91 days)
#' @param climval vector of climate data
#' @return mean value of climval
#' @noRd
qtr2 <- function(i, int, climval) {
  dclim <- c(climval, climval)
  sclim <- sum(dclim[i: (i + int)], na.rm = TRUE)
  return(sclim)
}

#' function to calculate the sum of climate values for a defined period
#' @param i starting index position in climate value in `climval`
#' @param period number of days
#' @param climval vector of climate data
#' @return mean value of climval
#' @noRd
qtr3 <- function(i, period, climval) {
  dclim <- c(climval, climval)
  sclim <- sum(dclim[i: (i + (period-1))], na.rm = TRUE)
  return(sclim)
}
