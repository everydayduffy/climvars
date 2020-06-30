#' gseason_temp: Calculate the growing season period as determined by
#' temperature values.
#'
#' @description `gseason_temp` calculates growing season period where
#' temperatures are above 'lower' and below 'upper' specified limits.
#'
#' @param temps a vector of temperature values.
#' @param tme a `POSIXlt` object representing the date and time of each
#' `temps` value.
#' @param lower integer specifying lower temperature limit of growing season.
#' @param upper integer specifying upper temperature limit of growing season.
#' @param nday specifies the number of consecutive days of temperatures above
#' 'lower' or below 'upper' before growing season start/end is accepted.
#'
#' @return A time-series object of binary values, where 1 indicates growing
#' season and 0 indicates not growing season.
#' @export
#'
#' @details To satisfy the requirements for `tme`, a POSIXlt object can be
#' created using the `tmecreate` wrapper function.
#'
#' @seealso [tmecreate()] can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' gseast <- gseason_temp(temps = hourly_temps, tme)
#' plot(gseast)
#'

gseason_temp <- function(temps, tme, lower = 5, upper = 35, nday = 5) {
  if (length(unique(tme$year)) > 1) warnb()
  if (is.na(sd(prec, na.rm = TRUE)) | is.na(sd(evap, na.rm = TRUE))) {
    gst <- NA
  } else {
    dint <- (24 * 3600) / (as.numeric(tme[2]) - as.numeric(tme[1]))
    # moving window mean of temps
    smooth_temp <- filter(temps, filter = rep(1/(nday*dint), nday*dint), sides = 2,
                  circular = TRUE)
    gst <- ifelse(smooth_temp > lower, 1, 0)
    gst <- ifelse(smooth_temp > upper, 0, gst)

  }
  return(gst)
}

