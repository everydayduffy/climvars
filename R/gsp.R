#' gsp: Calculate total precipitation during growing season.
#'
#' @description `gsp` calculates total precipitation during the growing season.
#'
#' @param prec a vector of precipitation values.
#' @param gs a vector of binary values indicating growing conditions
#' (1) = yes, (0) = no.
#'
#' @return a single numeric value of the total precipitation during growing
#' conditions.
#' @export
#'
#' @seealso the [gseason()] function can be used to create an array of growing
#' conditions (1) = yes, (0) = no accounting for temperature, precipitation and
#' optionally daylight hours.
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
#' totalgsp <- gsp(prec, gs)
#'
gsp <- function(prec, gs) {
  if(length(prec)!=length(gs)) stop("Precipitation and growing season vectors are different lengths")
  precg <- prec * gs
  precg[precg==0] <- NA
  sgsp <- sum(precg, na.rm = TRUE)
  return(sgsp)
}
