#' bio3: Calculate isothermality.
#'
#' @description `bio3` is used to calculate isothermality (day-to-night
#' temperature oscillations relative to annual oscillations).
#'
#' @param temps a vector of temperatures, normally for one year (see details).
#' @param tme a `POSIXlt` object representing the date and time of each `temps`
#' value.
#' @param min_percentile a value between 0-1 used to define the minimum
#' percentile.
#' @param max_percentile a value between 0-1 used to define the maximum
#' percentile.
#'
#' @return a single numeric value representing isothermality for each year.
#' @export
#'
#' @details bio3 is calculated using the mean of daily maximum and minimum
#' temperature. If data spans more than one year, data are aggregated by unique
#' month irrespective of year and one value returned.
#'
#' @seealso [bio2()] and [bio7()] for calculating diurnal and annual
#' temperature ranges. [tmecreate()] for creating a 'POSIXlt' object.
#'
#' @examples
#' temps <- 10 * sin(c(0:1459) / (pi * 150)) + rnorm(1460)
#' tme <- tmecreate(2010, 6)
#' plot(temps~as.POSIXct(tme), type = "l", xlab = "Month", ylab = "Temperature")
#' bio3(temps, tme)

bio3 <- function(temps, tme, min_percentile = 0.01, max_percentile = 0.99) {
  if (is.na(sd(temps, na.rm = TRUE)))
    tiso <- NA
  else {
    if (length(unique(tme$year)) > 1) warnb()
    b2 <- bio2(temps, tme, min_percentile = min_percentile,
               max_percentile = max_percentile)
    b7 <- bio7(temps, tme, min_percentile = min_percentile,
               max_percentile = max_percentile)
    tiso <- b2 / b7
  }
  return(tiso)
}
