#' first_autumn_frost: Identify date of first Autumn frost.
#'
#' @description `first_autumn_frost` is used to find the date of the first
#' Autumn frost.
#'
#' @param temps a vector of temperatures, spanning a year.
#' @param tme a `POSIXlt` object representing the date and time of each
#' `temps` value.
#' @param northern_hemisphere a logical indicating whether Spring dates are to
#' be calculated for the northern (TRUE) or southern (FALSE) hemisphere.
#' @return a POSIXlt object representing the date of the first Autumn frost, or
#' NA if no frost occurred.
#' @export
#'
#' @details Autumn dates are determined based on the hemisphere chosen. For the
#' northern hemisphere Autumn is assumed to begin 1st March and end on 31st May.
#' For the southern hemisphere, Spring is assumed to begin on 1st September and
#' finish on 30th November. If no frosts occurred within the Autumn period, NA
#' is returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' temps <- hourly_temps - 5
#' tme <- tmecreate(2010, 1)
#' plot(hourly_temps~as.POSIXct(tme), type = "l", xlab = "Month",
#' ylab = "Temperature")
#' first_autumn_frost(temps, tme)
#'

first_autumn_frost <- function(temps, tme, northern_hemisphere = TRUE) {

  if (length(unique(tme$year)) > 1) {
    stop("Data span more than one year.")
  }
  yr <- unique(tme$year) + 1900
  step <- as.numeric(tme[2])-as.numeric(tme[1])

  if(northern_hemisphere == TRUE) {
    aust <- as.POSIXlt(paste0(as.character(yr),"-09-01 00:00:00"))
    auen <- as.POSIXlt(paste0(as.character(yr),"-12-01 00:00:00"))
  } else {
    aust <- as.POSIXlt(paste0(as.character(yr),"-03-01 00:00:00"))
    auen <- as.POSIXlt(paste0(as.character(yr),"-06-01 00:00:00"))
  }
  autemps <- temps[tme >= aust & tme <= auen]
  if(min(autemps) >= 0) {
    message("No Autumn frosts identified.")
    faf <- NA
  } else {
    ind <- head(which(autemps < 0), n = 1)
    faf <- aust + (step * ind)
    faf <- round(faf, units = "day")
  }
  return(faf)
}
