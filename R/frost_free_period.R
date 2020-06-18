#' frost_free_period: Calculate the length of the longest frost free period.
#'
#' @description `frost_free_period` is used to find the length of a frost free
#' period, for a given season or yearly.
#'
#' @param temps a vector of temperatures, spanning a year.
#' @param tme a `POSIXlt` object representing the date and time of each
#' `temps` value.
#' @param period character vector indicating either year or custom.
#' @param pstart a `POSIXlt` object representing the date and time of the start
#' of a user-defined period. Only used if period = "custom".
#' @param pend a `POSIXlt` object representing the date and time of the start of
#' a user-defined period. Only used in period = "custom".
#' @return a single numeric value indicating the number of time steps (as
#' determined by `tme`) that are frost free.
#' @export
#'
#' @details values are returned at the time step of input data.
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#' @examples
#' temps <- hourly_temps - 5
#' tme <- tmecreate(2010, 1)
#' plot(hourly_temps~as.POSIXct(tme), type = "l", xlab = "Month",
#' ylab = "Temperature")
#' frost_free_period(temps, tme)
#'
frost_free_period <- function(temps, tme, period = "year", pstart = NA,
                              pend = NA) {

  if (length(unique(tme$year)) > 1) {
    stop("Data span more than one year.")
  }
  yr <- unique(tme$year) + 1900
  step <- as.numeric(tme[2])-as.numeric(tme[1])

  if(period == "year") {
    temps[temps >= 0] <- 1
    temps[temps < 0] <- 0
    # calculate runs of equal values (how many 1s/0s in a row)
    trle <- rle(temps)
    ffp <- max(trle[[1]][which(trle[[2]]==1)])
  } else if(period == "custom") {
    if(is.na(pstart)) stop("Custom start date `pstart` not provided.")
    if(is.na(pend)) stop("Custom end date `pend` not provided.")
    ptemps <- temps[tme >= pstart & tme <= pend]
    ptemps[ptemps >= 0] <- 1
    ptemps[ptemps < 0] <- 0
    # calculate runs of equal values (how many 1s/0s in a row)
    trle <- rle(ptemps)
    ffp <- max(trle[[1]][which(trle[[2]]==1)])
  }
  return(ffp)
}
