#' gseason_length: Calculate length of growing season.
#'
#' @description `gseason_length` calculates the length of the growing season
#' period.
#'
#' @param gs a vector of binary values indicating growing conditions
#' (1) = yes, (0) = no.
#' @param tme a `POSIXlt` object representing the date and time of each
#' `gs` value.
#' @param unit a character vector, either "day" or "hour" defining the desired
#' temporal resolution of the value returned.
#'
#' @return a single numeric value indicating the length of the growing season
#' to the nearest hour or day depending on `unit`.
#' @export
#'
#' @details If `unit` = "day" and data are a finer temporal resolution, then
#' the returned value is rounded to the nearest day.
#'
#' @seealso the [gseason()] function can be used to create an array of growing
#' conditions (1) = yes, (0) = no accounting for temperature, precipitation and
#' daylight hours.
#'
#' @examples
#' tme <- tmecreate(2010,1)
#' lon <- -5
#' lat <- 52
#' gs <- gseason(hourly_temps, hourly_precip, hourly_pev, tme, tme, tme, lon,
#' lat)
#'
#' gsl <- gseason_length(gs, tme, unit = "day")
#' gsl <- gseason_length(gs, tme, unit = "hour")
#'
gseason_length <- function(gs, tme, unit = "day")  {
  if (length(unique(tme1$year)) > 1) stop("Data span more than one year.")

  dint <- (24 * 3600) / (as.numeric(tme[2]) - as.numeric(tme[1]))
  if(unit == "day") {
    gsl <- round(sum(gs[gs == 1])/dint,0)
  } else if(unit == "hour") {
    if(dint != 24) stop("Hourly units selected, but data are not hourly, select daily instead.")
    gsl <- sum(gs[gs == 1])
  }
  return(gsl)
}
