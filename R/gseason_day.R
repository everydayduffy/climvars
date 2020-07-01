#' gseason_day: Determine daytime hours.
#'
#' @description `gseason_day` is used to calculate daytime (1) and non-daytime
#' (0) hours.
#'
#' @param tme a `POSIXlt` object.
#' @param lat latitude of the location for which daytime and non-daytime hours
#' are required.
#' @param lon longitude of the location for which daytime and non-daytime
#' hours are required.
#' @param merid an optional numeric value representing the longitude (decimal
#' degrees) of the local time zone meridian (0 for GMT). Default is 0.
#' @param dst an optional numeric value representing the time difference from
#' the timezone meridian (hours, e.g. +1 for BST if merid = 0). Default is 0.
#'
#' @importFrom microclima julday
#'
#' @return a vector of binary values indicating day (1) or night (0).
#' @export
#'
#' @seealso the [tmecreate()] function can be used to create a POSIXlt object.
#' @seealso requires wrapper function `sisimple` to be loaded.
#'
#' @examples
#' # daytime hours in Porthleven, Cornwall for 2019.
#' tme <- tmecreate(2019, 1)
#' gseason_day(tme, 50.08, -5.31)
#'
gseason_day <- function(tme, lat, lon, merid = 0, dst = 0) {
  jd <- microclima::julday(tme$year + 1900, tme$mon + 1, tme$mday)
  lt <- tme$hour + tme$min / 60 + tme$sec / 3600
  si <- sisimple(lt, lat, lon, jd, merid, dst)
  gsd <- ifelse(si > 0, 1, 0)
  return(gsd)
}
