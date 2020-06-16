#' frost_hours: Calculate frost hours.
#'
#' @description `frost_hours` is used to calculate the number of frost hours for
#' a given period of time.
#'
#' @param temps a vector of hourly temperatures.
#' @param tme a `POSIXlt` object representing the date and time of each
#' `temps` value.
#' @return a single numeric value of mean annual temperature.
#' @export
#'
#' @details Frost hours are defined as temperature values that fall below 0
#' degrees celcius.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' plot(hourly_temps~as.POSIXct(tme), type = "l", xlab = "Month",
#' ylab = "Temperature")
#' frost_hours(hourly_temps, tme)
#'

frost_hours <- function(temps, tme) {
  if(as.numeric(tme[2] - as.numeric(tme[1])) != 3600) {
    stop("Temperature data is not hourly.")
  }
  if(length(temps) != length(tme)) {
    stop("Temperature and time data are differing lengths.")
  }
  if (is.na(sd(temps, na.rm = TRUE))) {
    fh <- NA
  }
  else {
    ft <- temps[temps < 0]
    fh <- length(ft)
  }
  return(fh)
}
