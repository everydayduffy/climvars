#' bio14: Calculate precipitation of the driest period.
#'
#' @description `bio14` is used to calculate the total precipitation in the
#' driest period of the year.
#'
#' @param prec a vector of precipitation values, normally for one year (see
#' details).
#' @param tme a `POSIXlt` object representing the date and time of each `prec`
#' value.
#' @param period a single value defining the number of days for which total
#' precipitation is calculated for.
#'
#' @return a single numeric value of total precipitation in the driest period
#' of the year.
#' @export
#'
#' @details The total precipitation in a given period starting at each data
#' point is calculated, and the driest returned. By default the period is set
#' to 7 days. If data span more than one year, data across the whole period are
#' considered and a single value returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' plot(hourly_precip~as.POSIXct(tme), type = "l", xlab = "Month",
#' ylab = "Precipitation")
#' bio14(hourly_precip, tme)
#' bio14(hourly_precip, tme, period = 30)

bio14 <- function(prec, tme, period = 7) {
  if (is.na(sd(prec, na.rm = TRUE)))
    wp <- NA
  else {
    dprec <- aggregate(prec, by = list(tme$yday), FUN = sum, na.rm = TRUE)$x
    pprd <- sapply(c(1:length(dprec)), qtr3, period, dprec) # for each period work out sum precip for period
    wp <- min(pprd)
  }
  return(wp)
}
