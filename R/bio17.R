#' bio17: Calculate precipitation of the driest quarter.
#'
#' @description `bio17` is used to calculate the total precipitation in the
#' driest quarter of the year
#'
#' @param prec a vector of precipitation values, normally for one year (see
#' details).
#' @param tme a `POSIXlt` object representing the date and time of each `prec`
#' value.
#'
#' @return a single numeric value of total precipitation of the driest quarter.
#' @export
#'
#' @details Precipitation in quarter is calculated and total
#' precipitation in the driest quarter returned. If data span more than one
#' year, calculations are performed on all data and single value returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' prec <- (10 * sin(c(0:364) * (pi / -360)) + rnorm(365) + 12)
#' tme <- tmecreate(2010, 24)
#' plot(prec~as.POSIXct(tme), type = "l", xlab = "Month",
#' ylab = "Precipitation")
#' bio17(prec, tme)

bio17 <- function(prec, tme) {
  if (is.na(sd(prec, na.rm = TRUE)))
    pdry <- NA
  else {
    if (length(unique(tme$year)) > 1) warnb()
    qtr <- function(i, int) {
      pw <- c(prec, prec)
      su <- sum(pw[i: (i + int)], na.rm = TRUE)
      su
    }
    id <- (as.numeric(tme[2]) - as.numeric(tme[1])) / 86400
    int <- 91 / id
    dq <- sapply(c(1:length(prec)), qtr, int)
    i <- which(dq == min(dq, na.rm = TRUE))[1]
    pre2 <- c(prec, prec)
    pdry <- sum(pre2[i:(i + int)], na.rm = TRUE)
  }
  return(pdry)
}
