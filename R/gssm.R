#' gssm: Calculate mean soil moisture during the growing season.
#'
#' @description `gssm` calculates mean temperature during the growing season.
#'
#' @param sm a vector of soil moisture values (e.g. calculated by
#' `soil_moisture`).
#' @param gs a vector of binary values indicating growing conditions
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
#' @details `sm` must be the same length as `gseason`.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' evap <- rep(1.15,length(tme))
#' sm <- soil_moisture(hourly_precip, evap, 0.9)
#' gssm(sm, gs)

gssm <- function(sm, gs) {
  if(length(sm)!=length(gs)) stop("Soil moisture and growing season vectors are different lengths.")
  smg <- sm * gs
  smg[smg==0] <- NA
  mgssm <- mean(smg, na.rm = TRUE)
  return(mgssm)
}
