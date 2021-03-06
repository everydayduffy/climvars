#' bio16: Calculate precipitation of the wettest quarter.
#'
#' @description `bio16` is used to calculate the total precipitation of the
#' wettest quarter of the year.
#'
#' @param prec a vector of precipitation values, normally for one year (see
#'  details).
#' @param tme a `POSIXlt` object representing the date and time of each `prec`
#' value.
#'
#' @return a single numeric value of total precipitation of the wettest quarter.
#' @export
#'
#' @details Precipitation for each quarter is calculated and total
#' precipitation in the wettest quarter returned. If data span more than one
#' year, calculations are performed on all data and a single value returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' plot(hourly_precip~as.POSIXct(tme), type = "l", xlab = "Month",
#' ylab = "Precipitation")
#' bio16(hourly_precip, tme)

bio16 <- function(prec, tme) {
  if (is.na(sd(prec, na.rm = TRUE)))
    pwet <- NA
  else {
    if (length(unique(tme$year)) > 1) warnb()
    id <- (as.numeric(tme[2]) - as.numeric(tme[1])) / 86400
    int <- 91 / id
    wq <- sapply(c(1:length(prec)), qtr2, int)
    i <- which(wq == max(wq, na.rm = TRUE))[1]
    pre2 <- c(prec, prec)
    pwet <- sum(pre2[i:(i + int)], na.rm = TRUE)
  }
  return(pwet)
}
