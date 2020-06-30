#' tsp: Calculate total summer precipitation.
#'
#' @description `tsp` calculates total precipitation during the summer season.
#'
#' @param prec a vector of precipitation values.
#' @param tme a `POSIXlt` object representing the date and time of each
#' `prec` value.
#' @param northern_hemisphere a logical indicating whether Spring dates are to
#' be calculated for the northern (TRUE) or southern (FALSE) hemisphere.
#'
#' @return a single numeric value of the total precipitation during summer.
#' @export
#'
#' @details
#'
#' @examples
#' tme <- tmecreate(2010, 1)
#' tsp(hourly_precip, tme)
#' tsp(hourly_precip, tme, northern_hemisphere = FALSE)
#'

tsp <- function(prec, tme, northern_hemisphere = TRUE) {
  if (length(unique(tme$year)) > 1) {
    stop("Data span more than one year.")
  }
  yr <- unique(tme$year) + 1900

  if(northern_hemisphere == TRUE) {
    spst <- as.POSIXlt(paste0(as.character(yr),"-06-01 00:00:00"))
    spen <- as.POSIXlt(paste0(as.character(yr),"-09-01 00:00:00"))

    spprec <- prec[tme >= spst & tme <= spen]
  } else {
    # have to split southern summer in two
    spst1 <- as.POSIXlt(paste0(as.character(yr),"-12-01 00:00:00"))
    spen1 <- as.POSIXlt(paste0(as.character(yr),"-12-31 23:00:00"))
    spst2 <- as.POSIXlt(paste0(as.character(yr),"-01-01 00:00:00"))
    spen2 <- as.POSIXlt(paste0(as.character(yr),"-03-01 00:00:00"))

    spprec1 <- prec[tme >= spst1 & tme <= spen1]
    spprec2 <- prec[tme >= spst2 & tme <= spen2]
    spprec <- c(spprec1, spprec2)
  }
  psummer <- sum(spprec)
  return(psummer)
}

