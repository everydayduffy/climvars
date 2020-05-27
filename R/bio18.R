#' bio18: Calculate precipitation of the warmest quarter.
#'
#' @description `bio18` is used to calculate the total precipitation in the
#' warmest quarter of the year.
#'
#' @param temps a vector of temperature values, normally for one year (see
#' details).
#' @param prec a vector of precipitation values, normally for one year (see
#' details).
#' @param tme1 a `POSIXlt` object representing the date and time of each `temps`
#' value.
#' @param tme2 a `POSIXlt` object representing the date and time of each `prec`
#' value.
#'
#' @return a single numeric value of total precipitation in the warmest quarter
#' of the year.
#' @export
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @details If `temps` and `prec` vary in their temporal resolution, then they
#' are harmonised by aggregation (mean for temperature or sum for precipitation).
#' Then, each data point is treated as the start of a 91 day period for which
#' summed temperature is calculated. Once the warmest 91 day period is
#' identified, the total precipitation for the same period is calculated and a
#' single value returned. If data span more than one year, calculations are
#' performed on all data and a single value returned.
#'
#' @examples
#' # hourly
#' temps <- 10 * sin(c(0:8759) / (pi * 900)) + rnorm(8760)
#' tme1 <- tmecreate(2010, 1)
#' # 6-hourly
#' temps <- 10 * sin(c(0:1459) / (pi * 150)) + rnorm(1460)
#' tme1 <- tmecreate(2010, 6)
#' # daily
#' temps <- 10 * sin(c(0:364) / (pi * 37.5)) + rnorm(365)
#' tme1 <- tmecreate(2010, 24)
#' plot(temps~as.POSIXct(tme), type = "l", xlab = "Month", ylab = "Temperature")
#' # hourly
#' prec <- (10 * sin(c(0:8759) * (pi / -9000)) + rnorm(8760) + 14)
#' tme2 <- tmecreate(2010, 1)
#' # 6-hourly
#' prec <- (10 * sin(c(0:1459) * (pi / -1500)) + rnorm(1460) + 14)
#' tme2 <- tmecreate(2010, 6)
#' # daily
#' prec <- (10 * sin(c(0:364) * (pi / -360)) + rnorm(365) + 14)
#' tme2 <- tmecreate(2010, 24)
#' plot(prec~as.POSIXct(tme2), type = "l", xlab = "Month",
#' ylab = "Precipitation")
#' bio18(temps, prec, tme1, tme2)

bio18 <- function(temps, prec, tme1, tme2) {
  if (is.na(sd(prec, na.rm = TRUE)) | is.na(sd(temps, na.rm = TRUE)))
    pwarm <- NA
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
      sq <- rep(1:length(tme2), each = id2/id)
      prec <- aggregate(prec, by = list(sq), FUN = sum, na.rm = TRUE)$x
      time_step <- id
    } else {
      time_step <- id
    }
    # function to calculate total temps over x period
    temp_calc <- function(i, period, id, temps) {
      temp_mod <- c(temps, temps)
      temp_sum <- sum(temp_mod[i: (i + (period * id) - 1)], na.rm = TRUE)
    }
    # 3 monthly sums for each prec value
    sum_temps <- sapply(c(1:length(temps)), FUN = temp_calc, period = period,
                        id = time_step, temps = temps)
    warm_id <- which(sum_temps == max(sum_temps, na.rm = TRUE))[1]
    prec2 <- c(prec,prec)
    pwarm <- sum(prec2[warm_id:(warm_id + period * time_step)], na.rm = TRUE)
  }
  return(pwarm)
}
