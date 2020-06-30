#' bio10: Calculate mean temperature of the warmest quarter.
#'
#' @description `bio10` is used to calculate the mean temperature of the warmest
#' quarter of the year.
#'
#' @param temps a vector of temperatures, normally for one year (see details).
#' @param tme a `POSIXlt` object representing the date and time of each `temps`
#' value.
#'
#' @return a single numeric value of mean temperature in the warmest quarter of
#' the year.
#' @export
#'
#' @details Mean temperature of the warmest 3-month (91-day) period is
#' calculated from annual temperature values. If data span more than one year,
#' calculations are performed on all data and a single value returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' plot(hourly_temps~as.POSIXct(tme), type = "l", xlab = "Month",
#' ylab = "Temperature")
#' bio10(hourly_temps, tme)

bio10 <- function(temps, tme) {
  if (is.na(sd(temps, na.rm = TRUE)))
    thot <- NA
  else  {
    if (length(unique(tme$year)) > 1) warnb()
    qtr <- function(i, int) {
      tw <- c(temps, temps)
      me <- mean(tw[i: (i + int)], na.rm = TRUE)
      me
    }
    id <- (as.numeric(tme[2]) - as.numeric(tme[1])) / 86400
    int <- 91 / id
    hq <- sapply(c(1:length(temps)), qtr, int)
    i <- which(hq == max(hq, na.rm = TRUE))[1]
    tte <- c(temps, temps)
    thot <- mean(tte[i:(i + int)], na.rm = TRUE)
  }
  return(thot)
}
