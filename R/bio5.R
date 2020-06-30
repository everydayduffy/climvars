#' bio5: Calculate maximum temperature of the year.
#'
#' @description `bio5` is used to calculate the maximum temperature of the year.
#'
#' @param temps a vector of temperatures, normally for one year (see details).
#' @param tme a `POSIXlt` object representing the date and time of each
#' `temps` value.
#' @param percentile a value between 0-1 used to define the maximum percentile.
#'
#' @return a single numeric value of maximum temperature for a year.
#' @export
#'
#' @details The maximum temperature (percentile dependant) across all values is
#' returned. By default, percentile is set to 99%. If data spans more than one
#' year then a single value considering all data is returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' plot(hourly_temps~as.POSIXct(tme), type = "l", xlab = "Month", ylab = "Temperature")
#' bio5(hourly_temps, tme)
#' bio5(hourly_temps, tme, percentile = 0.95)

bio5 <- function(temps, tme, percentile = 0.99) {
  if (is.na(sd(temps, na.rm = TRUE)))
    tmx <- NA
    else {
      if (length(unique(tme$year)) > 1) warnb()
      tmx <- unname(quantile(temps, probs = percentile, na.rm = TRUE))
    }
  return(tmx)
}
