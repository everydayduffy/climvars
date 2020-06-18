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
#' @details `temps` must be the same length as `gseason`.
#'
#' @examples
#'temps <- hourly_temps
#'prec <- hourly_prec
#'evap <- prec + rnorm(length(prec)) - rnorm(length(prec))
#'
#'tme1 <- tmecreate(2010,1)
#'tme2 <- tmecreate(2010,1)
#'tme3 <- tmecreate(2010,1)
#'lon <- -5
#'lat <- 52
#'gs <- gseason(temps, prec, evap, tme1, tme2, tme3, lon, lat)
#'meangst <- gst(temps, gs)

gst <- function(temps, gs) {
  if(length(temps)!=length(gs)) stop("Temperature and growing season are different lengths")
  tempg <- temps * gs
  tempg[tempg==0]<-NA
  mgst <- mean(tempg, na.rm = TRUE)
  return(mgst)
}
