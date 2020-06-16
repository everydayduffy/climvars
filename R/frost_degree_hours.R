#' frost_degree_hours: Calculate frost degree hours.
#' #'
#' @description `frost_hours` is used to calculate the number of frost hours for
#' a given period of time.
#'
#' @param temps a vector of hourly temperatures.
#' @param tme a `POSIXlt` object representing the date and time of each
#' `temps` value.
#' @return a single numeric value of frost degree hours.
#' @export
#'
#' @details For each provided hourly temperature value, the number of degrees
#' celcius below 0 are calculated. These values are then summed to provide the
#' frost degree hours over a given period of time.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' plot(hourly_temps~as.POSIXct(tme), type = "l", xlab = "Month",
#' ylab = "Temperature")
#' frost_degree_hours(hourly_temps, tme)
#'
frost_degree_hours <- function(temps, tme) {
  if(as.numeric(tme[2] - as.numeric(tme[1])) != 3600) {
    stop("Temperature data are not hourly.")
  }
  if(length(temps) != length(tme)) {
    stop("Temperature and time data are differing lengths.")
  }
  if (is.na(sd(temps, na.rm = TRUE))) {
    fdh <- NA
  }
  else {
    ft <- temps[which(temps < 0)]
    ftf <- floor(ft)
    fdh <- abs(sum(ftf))
  }
  return(fdh)
}
