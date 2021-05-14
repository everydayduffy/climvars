#' bio4: Calculate temperature seasonality.
#'
#' @description `bio4` is used to calculate the differences in mean temperature
#' between the warmest and coldest consecutive three month (91 day) period.
#'
#' @param temps a vector of temperature values, normally for one year (see
#' details).
#' @param tme a `POSIXlt` object representing the date and time of each
#' `temps` value.
#'
#' @return a single numeric value representing annual temperature seasonality.
#' @export
#'
#' @details Each provided temperature data point is treated as the start of a 91
#' day period for which mean temperature is calculated. The differences between
#' consecutive 91 day means are then calculated, and the greatest absolute
#' difference returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' # hourly
#' tme1 <- tmecreate(2010, 1)
#' # 6-hourly
#' tme2 <- tmecreate(2010, 6)
#' # daily
#' tme3 <- tmecreate(2010, 24)
#' plot(hourly_temps~as.POSIXct(tme), type = "l", xlab = "Month", ylab = "Temperature")
#' bio4(hourly_temps, tme)
#' bio4(six_hourly_temps, tme2)
#' bio4(daily_temps, tme3)

bio4 <- function(temps, tme) {
  if (is.na(sd(temps, na.rm = TRUE)))
    tseas <- NA
    else {
      if (length(unique(tme$year)) > 1) warnb()
      period <- 91
      id <- 86400 / (as.numeric(tme[2]) - as.numeric(tme[1])) # num temps per day
      # 3 monthly means for each temp value
      mseas <- sapply(c(1:length(temps)), FUN = mean_climval, period = period,
                      id = id, climval = temps)
      # lagged differences
      diffs <- abs(diff(mseas, lag = period * id))
      tseas <- max(diffs, na.rm = TRUE)
    }
  return(tseas)
}
