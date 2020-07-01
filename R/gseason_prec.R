#' gseason_prec: Calculate the growing season period as determined by
#' precipitation and evapotranspiration balance.
#'
#' @description `gseason_prec` calculates growing season period where
#' precipitation exceeds half the potential evapotranspiration.
#'
#' @param prec a vector of precipitation values.
#' @param evap a vector of evapotranspiration values.
#' @param tme1 a `POSIXlt` object representing the date and time of each `prec`
#' value.
#' @param tme2 a `POSIXlt` object representing the date and time of each `evap`
#' value.
#' @param nday a single numeric value defining the number of days over which to
#' smooth the data.
#'
#' @return A time-series object of binary values, where 1 indicates growing
#' season and 0 indicates not growing season.
#' @export
#'
#' @details The growing season is defined as the period when precipitation
#' exceeds half the potential evapotranspiration. If `prec` and `evap` have
#' differing temporal resolutions, data are aggregated to the coarser of the
#' two. More about smoothing....
#'
#' @seealso [tmecreate()] can be used to create a POSIXlt object.
#'
#' @examples
#' tme1 <- tmecreate(2010,24)
#' tme2 <- tmecreate(2010,24)
#' prec <- (10 * sin(c(0:364) * (pi / -360)) + rnorm(365) + 15)
#' evap <- (10 * sin(c(0:364) * (pi / +360)) + rnorm(365) + 2)
#' gseason_prec(prec, evap, tme1, tme2)
#' gseason_prec(prec, evap, tme1, tme2, nday = 10)
#'

gseason_prec <- function(prec, evap, tme1, tme2, nday = 5) {
  if (length(unique(tme1$year)) > 1) warnb()
  if (length(unique(tme2$year)) > 1) warnb()
  if (is.na(sd(prec, na.rm = TRUE)) | is.na(sd(evap, na.rm = TRUE))) {
    gsp <- NA
  } else {
    id <- 86400 / (as.numeric(tme1[2]) - as.numeric(tme1[1]))
    id2 <- 86400 / (as.numeric(tme2[2]) - as.numeric(tme2[1]))
    # if evap is coarser tme than prec, then aggregate prec
    if(id2 < id) {
      sq <- rep(1:length(tme2), each = id/id2)
      prec <- aggregate(prec, by = list(sq), FUN = sum, na.rm = TRUE)$x
      time_step <- id2
      # if prec is coarser tme than evap, then aggregate evap
    } else if(id < id2) {
      sq <- rep(1:length(tme1), each = id2/id)
      evap <- aggregate(evap, by = list(sq), FUN = sum, na.rm = TRUE)$x
      time_step <- id
    } else {
      time_step <- id
    }
    # moving window mean of prec and evap
    smooth_prec <- stats::filter(prec, filter = rep(1/(nday*time_step),
                                                    nday*time_step), sides = 2,
                                 circular = TRUE)
    smooth_evap <- stats::filter(evap, filter = rep(1/(nday*time_step),
                                                    nday*time_step), sides = 2,
                                 circular = TRUE)
    # work out gseason for prec
    h_smooth_evap <- smooth_evap/2
    gsp <- smooth_prec
    gsp[which(smooth_prec <= h_smooth_evap)] <- 0
    gsp[which(smooth_prec > h_smooth_evap)] <- 1
  }
  return(as.vector(gsp))
}
