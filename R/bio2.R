#' bio2: Calculate mean annual diurnal temperature range.
#'
#' @description `bio2` is used to calculate the mean annual diurnal range in
#' temperature (range mean of the maximum-minimum).
#'
#' @param temps a vector of sub-daily temperatures, normally for one year (see
#' details).
#' @param tme a `POSIXlt` object representing the date and time of each `temps`
#' value.
#' @param min_percentile a value between 0-1 used to define the minimum
#' percentile.
#' @param max_percentile a value between 0-1 used to define the maximum
#' percentile.
#'
#' @return a single numeric value of mean diurnal temperature range.
#' @export
#'
#' @details The daily maximum (percentile dependant) and minimum (percentile
#' dependant) temperatures are calculated. The mean of the difference between
#' both is returned. By default, percentiles are set at 1% and 99% for the
#' minimum and maximum values respectively. If data spans more than one
#' year then a single value considering all data is returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' plot(hourly_temps~as.POSIXct(tme), type = "l", xlab = "Month", ylab = "Temperature")
#' bio2(hourly_temps, tme)
#' bio2(hourly_temps, tme, min_percentile = 0.05, max_percentile = 0.95)

bio2 <- function(temps, tme, min_percentile = 0.01, max_percentile = 0.99) {
  if (is.na(sd(temps, na.rm = TRUE)))
    tdtr <- NA
  else {
      if (length(unique(tme$year)) > 1) warnb()
      tmx <- aggregate(temps, by = list(tme$yday), FUN = quantile,
                       probs = max_percentile, na.rm = TRUE)$x
      tmn <- aggregate(temps, by = list(tme$yday), FUN = quantile,
                       probs = min_percentile, na.rm = TRUE)$x
      dtr <- tmx - tmn
      tdtr <- mean(dtr, na.rm = TRUE)
    }
  return(tdtr)
}
