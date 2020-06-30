#' bio7: Calculate annual temperature range.
#'
#' @description `bio7` is used to calculate the annual range in temperature.
#'
#' @param temps a vector of temperatures, normally for one year (see details).
#' @param tme a `POSIXlt` object representing the date and time of each `temps`
#' value.
#' @param min_percentile a value between 0-1 used to define the minimum
#' percentile.
#' @param max_percentile a value between 0-1 used to define the maximum
#' percentile.
#'
#' @return a single numeric value of annual temperature range (maximum-minimum
#' temperature values).
#' @export
#'
#' @details The range is calculated by subtracting the minimum from the maximum
#' temperature values for a year. By default, percentiles are set at 1% and 99%
#' for the minimum and maximum values respectively. If data spans more than one
#' year then a single value considering all data is returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' plot(hourly_temps~as.POSIXct(tme), type = "l", xlab = "Month", ylab = "Temperature")
#' bio7(hourly_temps, tme)
#' bio7(hourly_temps, tme, min_percentile = 0.05, max_percentile = 0.95)

bio7 <- function(temps, tme, min_percentile = 0.01, max_percentile = 0.99) {
  if (is.na(sd(temps, na.rm = TRUE)))
    tanr <- NA
  else {
    if (length(unique(tme$year)) > 1) warnb()
    tmx <- unname(quantile(temps, probs = max_percentile, na.rm = TRUE))
    tmn <- unname(quantile(temps, probs = min_percentile, na.rm = TRUE))
    tanr <- tmx-tmn
  }
  return(tanr)
}
