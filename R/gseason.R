#'gseason: Calculate the period when plant growth is possible.
#'
#' @description `gseason` calculates total annual growing hours where
#' temperatures are within defined upper and lower limits and precipitation
#' exceeds half potential evapotranspiration.
#'
#' @param temps a vector of temperature values.
#' @param prec a vector of precipitation values.
#' @param evap a vector of evapotranspiration values.
#' @param tme1 a `POSIXlt` object representing the date and time of each `temps`
#' value.
#' @param tme2 a `POSIXlt` object representing the date and time of each `prec`
#' value.
#' @param tme3 a `POSIXlt` object representing the date and time of each `evap`
#' value.
#' @param lon a numeric value indicating the longitudinal position of data (in
#' decimal degrees).
#' @param lat a numeric value indicating the latitudinal position of data (in
#' decimal degrees).
#' @param lower defines lower temperature limit where plant growth ceases
#' (degrees Celcius) and growing season is switched off.
#' @param upper defines upper temperature limit where plant growth ceases
#' (degrees Celcius) and growing season is switched off.
#' @param t_nday a single numeric value defining the number of days over which
#' to smooth temperature.
#' @param p_nday a single numeric value defining the number of days over which
#' to smooth precipitation and evapotranspiration data.
#' @param daynight if TRUE, growing season is continuous over 24 hours, if
#' FALSE, only calculates growing season during the day.
#' @param merid an optional numeric value representing the longitude (decimal
#' degrees) of the local time zone meridian (0 for GMT). Default is 0.
#' @param dst an optional numeric value representing the time difference from
#' the timezone meridian (hours, e.g. +1 for BST if merid = 0). Default is 0.
#'
#' @return an array of binary values indicating growing conditions (1) = yes,
#' (0) = no.
#' @export
#'
#' @details The growing season period is defined as the period where
#' temperatures are >5 degree Celcius and <35 degree Celcius and precipitation
#' > 0.5 PET.
#'
#' @seealso [gseason_prec()] calculates period where precipitation exceeds half
#' evapotranspiration.
#' @seealso [gseason_temp()] calculates period where temperature are above lower
#' limit for plant growth and below upper limit for plant growth.
#' @seealso [gseason_day()] calculates day/night time.
#' @seealso [tmecreate()] can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2019,1)
#' lon <- -5
#' lat <- 52
#' gseason(hourly_temps, hourly_precip, hourly_pev, tme, tme, tme, lon, lat)
#' gseason(hourly_temps, hourly_precip, hourly_pev, tme, tme, tme, lon, lat,
#' daynight = FALSE)
#'
gseason <- function(temps, prec, evap, tme1, tme2, tme3, lon, lat, lower = 5,
                    upper = 35, t_nday = 5, p_nday = 28, daynight = FALSE,
                    merid = 0, dst = 0) {
  gtemp <- gseason_temp(temps, tme1, lower, upper, t_nday)
  gprec <- gseason_prec(prec, evap, tme2, tme3, p_nday)
  if (daynight == FALSE) {
    gdn <- gseason_day(tme1, lat, lon, merid, dst)
    gscombo <- gtemp * gprec * gdn
  } else {
    gscombo <- gtemp * gprec
  }
  return(gscombo)
}
