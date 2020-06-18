#' gsmax: Calculate maximum temperature during the growing season.
#'
#' @description Calculates maximum temperature during the growing season.
#'
#' @param temps a vector of temperature values.
#' @param gs a vector of binary values indicating growing conditions
#' (1) = yes, (0) = no.
#'
#' @return a single numeric value of the maximum temperature during growing
#' conditions.
#' @export
#'
#' @seealso the [gseason()] function can be used to create an array of growing
#' conditions (1) = yes, (0) = no accounting for temperature, precipitation and
#' daylight hours.
#'
#' @examples
#' temps <- hourly_temps
#' prec <- hourly_prec
#' evap <- prec + rnorm(length(prec)) - rnorm(length(prec))
#'
#' tme1 <- tmecreate(2010,1)
#' tme2 <- tmecreate(2010,1)
#' tme3 <- tmecreate(2010,1)
#' lon <- -5
#' lat <- 52
#' gs <- gseason(temps, prec, evap, tme1, tme2, tme3, lon, lat)
#' maxgst <- gsmax(temps, gs)
#'
gsmax <- function(temps, gs) {
  if(length(temps)!=length(gs)) stop("Temperature and growing season vectors are different lengths")
  tempg <- temps * gs
  tempg[tempg==0] < -NA
  mxgst <- max(tempg, na.rm = TRUE)
  return(mxgst)
}
