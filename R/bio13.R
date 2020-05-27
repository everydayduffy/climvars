#' bio13: Calculate precipitation of the wettest period.
#'
#' @description `bio13` is used to calculate the total precipitation in the
#' wettest period of the year.
#'
#' @param prec a vector of precipitation values, normally for one year (see
#' details).
#' @param tme a `POSIXlt` object representing the date and time of each `prec`
#' value.
#' @param period a single value defining the number of days for which total
#' precipitation is calculated for.
#'
#' @return a single numeric value of total precipitation in the wettest period
#' of the year.
#' @export
#'
#' @details The total precipitation in a given period starting at each data
#' point is calculated, and the wettest returned. By default the period is set
#' to 7 days. If data span more than one year, data across the whole period are
#' considered and a single value returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' prec <- (10 * sin(c(0:364) * (pi / -360)) + rnorm(365) + 12)
#' tme <- tmecreate(2010, 24)
#' plot(prec~as.POSIXct(tme), type = "l", xlab = "Month", ylab = "Precipitation")
#' bio13(prec, tme, period = 7)
#' bio13(prec, tme, period = 30)

bio13 <- function(prec, tme, period = 7) {
  if (is.na(sd(prec, na.rm = TRUE)))
    wp <- NA
  else {
    qtr <- function(i, period, prec) {
      prec_mod <- c(prec, prec)
      prec_sum <- sum(prec_mod[i: (i + (period-1))], na.rm = TRUE)
      return(prec_sum)
    }
    dprec <- aggregate(prec, by = list(tme$yday), FUN = sum, na.rm = TRUE)$x
    pprd <- sapply(c(1:length(dprec)), qtr, period, dprec)
    wp <- max(pprd)
  }
  return(wp)
}
