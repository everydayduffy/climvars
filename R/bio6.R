#' bio6: Calculate minimum temperature of the year.
#'
#' @description `bio6` is used to calculate the minimum temperature of the year.
#'
#' @param temps a vector of temperatures, normally for one year (see details).
#' @param tme a `POSIXlt` object representing the date and time of each `temps`
#' value.
#' @param percentile a value between 0-1 used to define the minimum percentile.
#'
#' @return a single numeric value of minimum temperature for a year.
#' @export
#'
#' @details The minimum temperature (percentile dependant) across all values is
#' returned. By default, percentile is set to 1%. If data spans more than one
#' year then a single value considering all data is returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' temps <- 10 * sin(c(0:1459) / (pi * 150)) + rnorm(1460)
#' tme <- tmecreate(2010, 6)
#' plot(temps~as.POSIXct(tme), type = "l", xlab = "Month", ylab = "Temperature")
#' bio6(temps, tme)

bio6 <- function(temps, tme, percentile = 0.01) {
  if (is.na(sd(temps, na.rm = TRUE)))
    tmn <- NA
  else {
    if (length(unique(tme$year)) > 1) warnb()
    tmn <- unname(quantile(temps, probs = percentile, na.rm = TRUE))
  }
  return(tmn)
}
