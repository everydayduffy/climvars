#' gseason_length: Calculate length of growing season.
#'
#' @description `gseason_length` calculates the length of the growing season
#' period.
#'
#' @param gs a vector of binary values indicating growing conditions
#' (1) = yes, (0) = no.
#' @param tme a `POSIXlt` object representing the date and time of each
#' `gs` value.
#' @param daynight if TRUE, growing season is continuous over 24 hours, if
#' FALSE, night time values are removed from `gs`.
#' @param lon a numeric value indicating the longitudinal position of data (in
#' decimal degrees). Only required if `daynight` = FALSE.
#' @param lat a numeric value indicating the latitudinal position of data (in
#' decimal degrees). Only required if `daynight` = FALSE.
#' @param merid an optional numeric value representing the longitude (decimal
#' degrees) of the local time zone meridian (0 for GMT). Default is 0.
#' Only required if `daynight` = FALSE.
#' @param dst an optional numeric value representing the time difference from
#' the timezone meridian (hours, e.g. +1 for BST if merid = 0). Default is 0.
#' Only required if `daynight` = FALSE.
#' @param init a logical indicating whether to define an initialisation period.
#' If TRUE `init_days` will be used to define the start of the growing season.
#' If FALSE, all values in `gs` will be considered.
#' @param init_days a single numeric value indicating the number of consecutive
#' growing season days required to mark the beginning of growing season.
#' @param interrupt a logival indicating whether to use interrupts. If TRUE,
#' then `n_interrupt` and `interrupt_days` are used.
#' @param n_interrupt a single numeric value indicating the number of interrupts
#' of length `interrupt_days` allowed before growing season ends.
#' @param interrupt_days a single number value indicating the length of
#' interrupt(s) allowed before growing season ends.
#' @param unit a character vector, either "day" or "hour" defining the desired
#' temporal resolution of the value returned.
#'
#' @return a single numeric value indicating the length of the growing season
#' to the nearest hour or day depending on `unit`.
#' @export
#'
#' @details If `unit` = "day" and data are a finer temporal resolution, then
#' the returned value is rounded to the nearest day.
#'
#' @seealso the [gseason()] function can be used to create an array of growing
#' conditions (1) = yes, (0) = no accounting for temperature, precipitation and
#' daylight hours.
#'
#' @examples
#' tme <- tmecreate(2019,1)
#' lon <- -5
#' lat <- 52
#' gs <- gseason(hourly_temps, hourly_precip, hourly_pev, tme, tme, tme, lon,
#' lat, daynight = FALSE)
#'
#' gsl <- gseason_length(gs, tme, daynight = FALSE, lon, lat,  unit = "day")
#' gsl <- gseason_length(gs, tme, daynight = FALSE, lon, lat,  unit = "hour")
#'
gseason_length <- function(gs, tme, daynight = FALSE, lon, lat, dst = 0,
                           merid = 0, init = TRUE, init_days = 30,
                           interrupt = TRUE, n_interrupt = 2,
                           interrupt_days = 1, unit = "day") {
  if (length(unique(tme$year)) > 1) stop("Data span more than one year.")

  dint <- (24 * 3600) / (as.numeric(tme[2]) - as.numeric(tme[1]))

  # day/night sorting
  if(daynight == FALSE) {
    gsd <- gseason_day(tme, lat, lon, merid, dst)
    # adjust gs by removing night values
    gs <- gs[-which(gsd == 0)]
  }
  # trim gs so that start is the start of user defined growing season
  if(init == TRUE) {
    # calculate runs of 0s and 1s
    gsrle <- rle(gs)
    # find start index of gs
    init_id <- which(gsrle$lengths >= init_days * dint & gsrle$values == 1)
    init_start_id <- sum(gsrle$lengths[1:init_id-1])
    gs <- gs[(init_start_id+1):length(gs)]
  }
  # apply interrupts
  if(interrupt == TRUE) {
    gsrle2 <- rle(gs)
    # id of non-allowed interrupts (too big)
    big_int_id <- which(gsrle2$lengths > interrupt_days*dint & gsrle2$values == 0)
    # if there are any, trim the tail of the vector
    if(length(big_int_id!=0)) {
      big_int_start_id <- sum(gsrle2$lengths[1:big_int_id[1]-1])
      # trim gs
      gs <- gs[1:big_int_start_id]
    }
    # then check for interrupts (small enough)
    gsrle3 <- rle(gs)
    # id of allowed length interrupts
    int_id <- which(gsrle3$length <= interrupt_days*dint & gsrle3$values == 0)
    # if there are more allowed length interrupts than specified
    if(length(int_id) > n_interrupt) {
      small_int_start_id <- sum(gsrle3$lengths[1:int_id[interrupt+1]-1])
      # trim tail of the vector after too many interrupts
      gs <- gs[1:small_int_start_id]
    }
    # turn allowed interrupts to 1
    gs[gs == 0] <- 1
  }
  if(unit == "day") {
    if(daynight == FALSE) warning("Only daytime hours considered. Returned value is cumulative number of hours, represented as days.")
    gsl <- round(sum(gs[gs == 1])/dint,0)
  } else if(unit == "hour") {
    if(dint != 24) stop("Hourly units selected, but data are not hourly, select daily instead.")
    gsl <- sum(gs[gs == 1])
  }
return(gsl)
}
