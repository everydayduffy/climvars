#' temp_thresh: Calculate the number of temperature values above or below a
#' threshold.
#'
#' @description `temp_thresh` is used to calculate the number of input temperature
#' values that fall above or below a user-defined threshold.
#'
#' @param temps a vector of hourly temperatures.
#' @param tme a `POSIXlt` object representing the date and time of each
#' `temps` value.
#' @param thresh a single numeric value defining the temperature threshold.
#' @param above logical indicating whether values above or below threshold are
#' to be counted. Default is TRUE.
#' @return a single numeric value indicating the number of temperature values
#' above or below a threshold.
#' @export
#'
#' @details ...
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2019, 1)
#' plot(hourly_temps~as.POSIXct(tme), type = "l", xlab = "Month",
#' ylab = "Temperature")
#' temp_thresh(hourly_temps, tme, thresh = 5)
#' temp_thresh(hourly_temps, tme, thresh = -1, above = FALSE)
#'

temp_thresh <- function(temps, tme, thresh, above = TRUE) {
  if (is.na(sd(temps, na.rm = TRUE))) {
    tt <- NA
  } else {
    ttbin <- temps
    if(above == TRUE) {
      tt <- length(temps[temps > thresh])
    } else {
      tt <- length(temps[temps < thresh])
    }
    return(tt)
  }
}
