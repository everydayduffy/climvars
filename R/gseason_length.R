#' gseason_length: Calculate length of growing season.
#'
#' @description `gseason_length` calculates the length of the growing season
#' period.
#'
#' @param gs a vector of binary values indicating growing conditions
#' (1) = yes, (0) = no.
#' @param tme a `POSIXlt` object representing the date and time of each
#' `gs` value.
#' @param unit a character vector, either "day" or "hour" defining the desired
#' temporal resolution of the value returned.
#'
#' @return a single numeric value indicating the length of the growing season
#' to the nearest hour or day depending on `unit`.
#' @export
#'
#' @details If unit = "day" and data are a finer temporal resolution, then
#' the returned value is rounded to the nearest day.
#'
#' @seealso the [gseason()] function can be used to create an array of growing
#' conditions (1) = yes, (0) = no accounting for temperature, precipitation and
#' daylight hours.
#'
#'
gseasonlength <- function(gs, tme, unit = "day")  {
  if (length(unique(tme1$year)) > 1) stop("Data span more than one year.")

  if(unit = "day") {

    ???

  } else if(unit = "hour") {

    ???

  }

  return(gsl)
}


  i <- 1
  for (year in startyear:endyear) {
    print(year)
    styear[,,1] <- gsl(gseason, year)
    i <- i +1
  }
  gsl <- apply(styear,c(1,2),mean,na.rm=T)
  gsl
}

temps <- hourly_temps
prec <- hourly_prec
evap <- prec + rnorm(length(prec)) - rnorm(length(prec))

tme1 <- tmecreate(2010,1)
tme2 <- tmecreate(2010,1)
tme3 <- tmecreate(2010,1)
lon <- -5
lat <- 52
gs <- gseason(temps, prec, evap, tme1, tme2, tme3, lon, lat)


