#' ssm: Calculate mean summer soil moisture.
#'
#' @description `ssm` calculates average soil moisture content over the summer
#' period.
#'
#' @param sm a vector of soil moisture values (e.g. calculated by `soil_moisture`)
#' @param tme a `POSIXlt` object representing the date and time of each `sm`
#' value.
#' @param northern_hemisphere a logical indicating whether Spring dates are to
#' be calculated for the northern (TRUE) or southern (FALSE) hemisphere.
#'
#' @return a single numeric value of the total precipitation during summer.
#'
#' @details add info about the dates used for summer.
#'
#' @seealso [soil_moisture()] calculates volumetric soil water fraction of
#' soil.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' evap <- rep(1.15,length(tme))
#' sm <- soil_moisture(hourly_precip, evap, 0.9)
#' ssm(sm, tme)
#' ssm(sm, tme, northern_hemisphere = FALSE)

ssm <- function(sm, tme, northern_hemisphere = TRUE) {
  if (length(unique(tme$year)) > 1) {
    stop("Data span more than one year.")
  }
  yr <- unique(tme$year) + 1900

  if(northern_hemisphere == TRUE) {
    spst <- as.POSIXlt(paste0(as.character(yr),"-06-01 00:00:00"))
    spen <- as.POSIXlt(paste0(as.character(yr),"-09-01 00:00:00"))

    spsm <- sm[tme >= spst & tme <= spen]
  } else {
  # have to split southern summer in two
  spst1 <- as.POSIXlt(paste0(as.character(yr),"-12-01 00:00:00"))
  spen1 <- as.POSIXlt(paste0(as.character(yr),"-12-31 23:00:00"))
  spst2 <- as.POSIXlt(paste0(as.character(yr),"-01-01 00:00:00"))
  spen2 <- as.POSIXlt(paste0(as.character(yr),"-03-01 00:00:00"))

  spsm1 <- prec[tme >= spst1 & tme <= spen1]
  spsm2 <- prec[tme >= spst2 & tme <= spen2]
  spsm <- c(spsm1, spsm2)
  }
  smsummer <- mean(spsm)
  return(smsummer)
}

