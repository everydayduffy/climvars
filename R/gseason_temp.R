#' gseason_temp: Calculate the growing season period as determined by
#' temperature.
#'
#' @description `gseason_temp` calculates growing season period where
#' temperatures are above 'lower' and below 'upper' specified limits.
#'
#' @param temps a vector of temperature values.
#' @param tme a `POSIXlt` object representing the date and time of each
#' `temps` value.
#' @param lower integer specifying lower temperature limit of growing season.
#' @param upper integer specifying upper temperature limit of growing season.
#' @param nday a single numeric value defining the number of days over which to
#' smooth the data.
#'
#' @return A time-series object of binary values, where 1 indicates growing
#' season and 0 indicates not growing season.
#' @export
#'
#' @details A filter is used to create a moving window mean of temperatures. The
#' extent of the filter is determined by `nday`.
#'
#' @seealso [tmecreate()] can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2019, 1)
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
    smooth_temp <- stats::filter(temps, filter = rep(1/(nday*dint), nday*dint), sides = 2,
                  circular = TRUE)
    gst <- ifelse(smooth_temp > lower, 1, 0)
    gst <- ifelse(smooth_temp > upper, 0, gst)

  }
  return(as.vector(gst))
}
