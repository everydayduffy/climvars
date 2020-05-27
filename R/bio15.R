#' bio15: Calculate precipitation seasonality.
#'
#' @description `bio15` is used to calculate the differences in mean
#' precipitation between the wettest and driest consecutive three month (91 day)
#' period.
#'
#' @param prec a vector of precipitation values, normally for one year (see
#' details).
#' @param tme a `POSIXlt` object representing the date and time of each `prec`
#' value.
#'
#' @return a single numeric value representing annual precipitation seasonality.
#' @export
#'
#' @details Each provided precipitation data point is treated as the start of a
#' 91 day period for which mean temperature is calculated. The differences
#' between consecutive 91 day means are then calculated, and the greatest
#' absolute difference returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' prec <- (10 * sin(c(0:364) * (pi / -360)) + rnorm(365) + 12)
#' tme <- tmecreate(2010, 24)
#' plot(prec~as.POSIXct(tme), type = "l", xlab = "Month",
#' ylab = "Precipitation")
#' bio15(prec, tme)
#'

bio15 <- function(prec, tme) {
  if (is.na(sd(prec, na.rm = TRUE)))
    tprec <- NA
    else {
      if (length(unique(tme$year)) > 1) warnb()
      period <- 91
      id <- 86400 / (as.numeric(tme[2]) - as.numeric(tme[1])) # num temps per day
      # function to calculate mean temps over x period
      mprec <- function(i, period, id, prec) {
        prec_mod <- c(prec, prec)
        prec_mean <- mean(prec_mod[i: (i + (period * id) - 1)], na.rm = TRUE)
      }
      # 3 monthly means for each temp var
      mseas <- sapply(c(1:length(prec)), FUN = mprec, period = period, id = id,
                      prec = prec)
      # lagged differences
      diffs <- abs(diff(mseas, lag = period * id))
      tprec <- max(diffs, na.rm = TRUE)
    }
  return(tprec)
}
