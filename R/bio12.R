#' bio12: Calculate total annual precipitation.
#'
#' @description `bio12` is used to calculate total precipitation in the year.
#'
#' @param prec a vector of precipitation values, normally for one year (see
#'  details).
#' @param tme a `POSIXlt` object representing the date and time of each `temps`
#' value.
#'
#' @return a single numeric value of total annual precipitation.
#' @export
#'
#' @details All precipitation values for each year are summed. If data span more
#' than one year, calculations are performed on all data and a single value
#' returned.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' plot(hourly_precip~as.POSIXct(tme), type = "l", xlab = "Month",
#' ylab = "Precipitation")
#' bio12(hourly_precip, tme)

bio12 <- function(prec, tme) {
  if (is.na(sd(prec, na.rm = TRUE)))
    map <- NA
  else {
    if (length(unique(tme$year)) > 1) warnb()
    map <- sum(prec, na.rm = TRUE) / length(unique(tme$year))
  }
  return(map)
}
