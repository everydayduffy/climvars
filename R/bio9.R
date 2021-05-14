#' bio9: Calculate mean temperature of the driest quarter.
#'
#' @description `bio9` is used to calculate the mean temperature of the driest
#' quarter of the year.
#'
#' @param temps a vector of temperatures, normally for one year (see details).
#' @param prec a vector of precipitation values, normally for one year (see
#' details).
#' @param tme1 a `POSIXlt` object representing the date and time of each `temps`
#' value.
#' @param tme2 a `POSIXlt` object representing the date and time of each `prec`
#' value.
#'
#' @return a single numeric value of mean temperature of the driest quarter of
#' the year.
#' @export
#'
#' @details If `temps` and `prec` vary in their temporal resolution, then they
#' are harmonised by aggregation (mean for temperature or sum for precipitation).
#' Then, each data point is treated as the start of a 91 day period for which
#' total precipitation is calculated. Once the driest  91 day period is
#' identified, the mean temperature for the same period is calculated and a
#' single value returned. If data span more than one year, calculations are
#' performed on all data and a single value returned.
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
#' plot(hourly_temps~as.POSIXct(tme1), type = "l", xlab = "Month",
#' ylab = "Temperature")
#' plot(hourly_precip~as.POSIXct(tme1), type = "l", xlab = "Month",
#' ylab = "Precipitation")
#' bio9(hourly_temps, hourly_precip, tme1, tme1)
#' bio9(hourly_temps, six_hourly_precip, tme1, tme2)
#' bio9(hourly_temps, daily_precip, tme1, tme3)

bio9 <- function(temps, prec, tme1, tme2) {
  if (is.na(sd(prec, na.rm = TRUE)) | is.na(sd(temps, na.rm = TRUE)))
    tdry <- NA
  else {
    if (length(unique(tme1$year)) > 1) warnb()
    if (length(unique(tme2$year)) > 1) warnb()
    period <- 91
    id <- 86400 / (as.numeric(tme1[2]) - as.numeric(tme1[1]))
    id2 <- 86400 / (as.numeric(tme2[2]) - as.numeric(tme2[1]))
    # if precip is coarser tme than temp, then aggregate temps
    if(tme2[2] - tme2[1] > tme1[2] - tme1[1]) {
      sq <- rep(1:length(tme2), each = id/id2)
      temps <- aggregate(temps, by = list(sq), FUN = mean, na.rm = TRUE)$x
      time_step <- id2
      # if temp is coarser tme than prec, then aggregate
    } else if(tme1[2] - tme1[1] > tme2[2] - tme2[1]) {
      sq <- rep(1:length(tme1), each = id2/id)
      prec <- aggregate(prec, by = list(sq), FUN = sum, na.rm = TRUE)$x
      time_step <- id
    } else {
      time_step <- id
    }
    # 3 monthly sums for each prec value
    sum_prec <- sapply(c(1:length(temps)), FUN = sum_climval, period = period,
                       id = time_step, climval = prec)
    dry_id <- which(sum_prec == min(sum_prec, na.rm = TRUE))[1]
    temps2 <- c(temps,temps)
    tdry <- mean(temps2[dry_id:(dry_id + period * time_step)], na.rm = TRUE)
  }
  return(tdry)
}
