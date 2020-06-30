#' bio1: Calculate mean annual temperature.
#'
#' @description `bio1` is used to calculate mean annual temperature.
#'
#' @param temps a vector of temperatures, normally for one year (see details).
#' @param tme a `POSIXlt` object representing the date and time of each
#' `temps` value. Ignored if method unspecified.
#' @param method an optional character string describing the method used to
#' calculate mean annual temperature. Options are "dailymaxmin" or unspecified
#' (see details).
#' @return a single numeric value of mean annual temperature.
#' @export
#'
#' @details If `method` is "dailymaxmin", daily mean temperatures are
#' calculated as the mean of daily maximum and minimum temperatures and annual
#' mean calculated from daily means. Otherwise the mean of `temps` is returned.
#' If using `dailymaxmin` method and data span more than one year, calculations
#' will be performed on all data and a single value returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' plot(hourly_temps~as.POSIXct(tme), type = "l", xlab = "Month", ylab = "Temperature")
#' bio1(hourly_temps, tme)
#' bio1(hourly_temps, tme, method = "dailymaxmin")

bio1 <- function(temps, tme, method = "") {
  if (is.na(sd(temps, na.rm = TRUE)))
    tmean <- NA
  else {
    if (method ==  "dailymaxmin") {
      if (length(unique(tme$year)) > 1) warnb()
      tmx <- aggregate(temps, by = list(tme$yday), max, na.rm = TRUE)$x
      tmn <- aggregate(temps, by = list(tme$yday), min, na.rm = TRUE)$x
      tme <- (tmx + tmn) /2
      tmean <- mean(tme, na.rm = TRUE)
    }
    else {
      if (length(unique(tme$year)) > 1) warna()
      tmean <- mean(temps, na.rm = TRUE)
    }
  }
  return(tmean)
}
