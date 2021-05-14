#' bio11: Calculate mean temperature of the coldest quarter.
#'
#' @description `bio11` is used to calculate the mean temperature of the coldest
#' quarter of the year.
#'
#' @param temps a vector of temperatures, normally for one year (see details).
#' @param tme a `POSIXlt` object representing the date and time of each `temps`
#' value.
#'
#' @return a single numeric value of mean temperature of the coldest quarter of
#' the year.
#' @export
#'
#' @details Mean temperature of the coldest 3-month (91-day) period is
#' calculated from annual temperature values. If data span more than one year,
#' calculations are performed on all data and a single value returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' plot(hourly_temps~as.POSIXct(tme), type = "l", xlab = "Month",
#' ylab = "Temperature")
#' bio11(hourly_temps, tme)

bio11 <- function(temps, tme) {
  if (is.na(sd(temps, na.rm = TRUE)))
    tcold <- NA
  else {
    if (length(unique(tme$year)) > 1) warnb()
    id <- (as.numeric(tme[2]) - as.numeric(tme[1])) / 86400
    int <- 91 / id
    cq <- sapply(c(1:length(temps)), qtr, int = int, climval = temps)
    i <- which(cq == min(cq, na.rm = TRUE))[1]
    tte <- c(temps, temps)
    tcold <- mean(tte[i:(i + int)], na.rm = TRUE)
  }
  return(tcold)
}

