#' mst: Calculate mean summer temperature.
#'
#' @description `mst` calculates the mean temperature during summer, accounting
#' for differences in the timing of the summer period for the northern and
#' southern hemispheres.
#'
#' @param temps a vector of temperature values.
#' @param tme a `POSIXlt` object representing the date and time of each
#' `prec` value.
#' @param northern_hemisphere a logical indicating whether Spring dates are to
#' be calculated for the northern (TRUE) or southern (FALSE) hemisphere.
#'
#' @return a single numeric value of the mean temperature during summer.
#' @export
#'
#' @details
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' mst(hourly_temps, tme)
#' mst(hourly_temps, tme, northern_hemisphere = FALSE)

mst <- function(temps, tme, northern_hemisphere = TRUE) {
  if (length(unique(tme$year)) > 1) {
    stop("Data span more than one year.")
  }
  yr <- unique(tme$year) + 1900

  if(northern_hemisphere == TRUE) {
    spst <- as.POSIXlt(paste0(as.character(yr),"-06-01 00:00:00"))
    spen <- as.POSIXlt(paste0(as.character(yr),"-09-01 00:00:00"))

    sptemps <- temps[tme >= spst & tme <= spen]

  } else {
    # have to split southern summer in two
    spst1 <- as.POSIXlt(paste0(as.character(yr),"-12-01 00:00:00"))
    spen1 <- as.POSIXlt(paste0(as.character(yr),"-12-31 23:00:00"))
    spst2 <- as.POSIXlt(paste0(as.character(yr),"-01-01 00:00:00"))
    spen2 <- as.POSIXlt(paste0(as.character(yr),"-03-01 00:00:00"))

    sptemps1 <- temps[tme >= spst1 & tme <= spen1]
    sptemps2 <- temps[tme >= spst2 & tme <= spen2]
    sptemps <- c(sptemps1, sptemps2)
  }
  mtsummer <- mean(sptemps)
  return(mtsummer)
}
