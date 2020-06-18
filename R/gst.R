#' gst: Calculate mean growing season temperature.
#'
#' @description `gst` calculates mean temperature during the growing season.
#'
#' @param temps a vector of temperature values.
#' @param gseason a vector of binary values indicating growing conditions
#' (1) = yes, (0) = no.
#'
#' @return a single numeric value of the mean temperature during growing
#' conditions.
#' @export
#'
#' @seealso the [gseason()] function can be used to create an array of growing
#' conditions (1) = yes, (0) = no accounting for temperature, precipitation and
#' optionally daylight hours.
#'
#' @examples
#' tme <- as.POSIXlt(c(0:1459) * 3600 * 6, origin = "2010-01-01 00:00", tz = "GMT")
#' gs <- gseason_day(tme, 6, 21)
#' gseason <- array(gs, dim=c(1, 1, 1460))
#' temp <- 10 * sin(c(0:1459) / (pi * 150)) + rnorm(1460)
#' meangst <- gst(temp, gseason)

#'temps <- hourly_temps
#'prec <- hourly_prec
#'evap <- prec + rnorm(length(prec)) - rnorm(length(prec))
#'
#'tme1 <- tmecreate(2010,1)
#'tme2 <- tmecreate(2010,1)
#'tme3 <- tmecreate(2010,1)
#'lon <- -5
#'lat <- 52
#'gs_bin <- gseason(temps, prec, evap, tme1, tme2, tme3, lon, lat)
#'meangst <- gst(temps, gs_bin)

gst <- function(temps, gs_bin) {
  tempg <- temps * gs_bin
  tempg[tempg==0]<-NA
  mgst <- mean(tempg, na.rm = TRUE)
  return(mgst)
}
