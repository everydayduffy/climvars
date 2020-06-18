#'sisimple: Calculates the solar index
#'
#'@description `sisimple` calculates the proportion of direct beam radiation incident on an inclined surface at a specified time and location.
#'
#'@param locatime local time (decimal hour, 24 hour clock).
#'@param lat latitude of the location for which the solar index is required (decimal degrees, -ve south of the equator).
#'@param long longitude of the location for which the solar index is required (decimal degrees, -ve west of Greenwich meridian).
#'@param julian Julian day expressed as an integer as returned by julday().
#'@param merid an optional numeric value representing the longitude (decimal degrees) of the local time zone meridian (0 for GMT). Default is 0.
#'@param dst an optional numeric value representing the time difference from the timezone meridian (hours, e.g. +1 for BST if merid = 0).
#'
#'@return a numeric value of the proportion of direct beam radiation on a horizontal surface at the specified latitude and longitude.
#'@export
#'
#'@seealso the microclima function `julday` can be used to derive `julian`.
#'
#'@examples
#'jd <- julday (2010, 6, 21)
#'si <- sisimple(11, 50, -5, jd, merid=0, dst=0)
#'
sisimple <- function(localtime, lat, long, julian, merid = 0, dst = 0) {
  saltitude <- solalt(localtime, lat, long, julian, merid, dst)
  alt <- saltitude * (pi / 180)
  index <- cos(pi / 2 - alt)
  index[index < 0] <- 0
  index
}

#'resamplearray: resamples an array to the dimensions of another array
#'
#'@description reformats data in an array and creates a new array objects to
#'  dimensions specified by `rin`.
#'
#'@param a the array object to be resampled.
#'@param rin a raster object used to resample extent of 'a' to extent of 'rout'.
#'@param rout a raster object with extent parameters that 'a' should be resampled to.
#'
#'@import raster
#'
#'@return an array with the same latitude and longitude as 'rout'.
#'@export
#'
#'@examples
#'originalarray <- array(rnorm(100), dim=c(94,192,1464))
#'rin <- raster(originalarray[,,1])
#'extent(rin) <- c(-0.9375, 359.0625, -89.49406, 89.49406)
#'routm <- matrix(rnorm(10512), 73, 144)
#'rout <- raster(routm)
#'extent(rout) <- c(-1.25, 358.75, -91.25, 91.25)
#'newarray <- resamplearray(a, rin, rout)
#'dim(originalarray)
#'dim(newarray)
#'
resamplearray <- function(a, rin, rout) {
  b <- brick(a)
  rin <- extend(rin, extent(rout))
  extent(b) <- extent(rin)
  b <- resample(b, rout)
  ao <- array(NA, dim = dim(b))
  for (i in 1:dim(b)[3])
    ao[,,i] <- getValues(raster(b, i), format = "matrix")
  ao
}
#'tmecreate: Creates a `POSIXlt` object representing calendar dates and times
#'
#'@description `tmecreate` is used to create a `POSIXlt` object representing
#'calendar dates and times for the years specified by `years` and for time
#'intervals specified by `hourint`.
#'
#'@param years a vector of years.
#'@param hourint a single numeric value representing the time interval in hours
#'   between dates and times.
#'
#'@return an object of class `POSIXlt` with calander dates and times.
#'@export
#'
#'@examples
#'head(tmecreate(2010, 24))
#'tail(tmecreate(2010, 6))
tmecreate<- function(years, hourint = 6) {
  lpfun <- function(year) {
    diy <- ifelse(year%%4 == 0, 366, 365)
    if (year%%100 == 0 & year%%400 != 0) diy<-365
    diy
  }
  diy <- sapply(years, lpfun)
  i <- sum(diy) * 24 / hourint -1
  tme <- as.POSIXlt(c(0:i) * 3600 * hourint, origin = paste0(min(years),
                                                             "-01-01 00:00"),
                    tz = "GMT")
  tme
}

#'submonthly'
#'
#'@description `submonthly` is used to create a list representing each measurement per half-month.
#'
#'@param tme a POSIXlt object representing calendar dates and times.
#'@param div numeric object specifying the value by which to divide total days of each month. Default = 2.
#'
#'@return a vector of numeric values alternating between 0 and 1.
#'@export
#'
#'@details If div=2, value changes after the last 'tme' measurement in each half-month.
#'
#'@seealso the [tmecreate()] function can be used to create a POSIXlt object.
#'
#'@examples
#'tme <- tmecreate(2010, 6)
#'sm <- submonthly(tme, div = 2)
#'sm2 <- submonthly(tme, div = 4)
submonthly <- function(tme, div = 2) {
  bwm <-0
  for (i in 1:12) {
    dm <-max(tme$mday[which(tme$mon == (i - 1))]) #max days in each month (e.g. Nov = 31). will account for leap years
    bw <- floor((tme$mday[which(tme$mon == (i - 1))] - 1)/ dm * div)
    bwm <- c(bwm, bw)
  }
  bwm[-1]
}
#'cumsumseq
#'#'@description `cumsumseq` is used to create a vector of values indicating
#'growing season conditions for the specified time period.
#'
#'@param gs a vector of binary values indicating growing season (1) or non-growing season (0).
#'@param ehour a vector of evapotranspiration values.
#'@param div numeric object specifying the value by which to divide total days
#'of each month.
#'
#'@return a vector of numeric values alternating between 0 and 1.
#'@export
#'
#'@examples
#'gs <- c(0,0,1,1,1,1,0,0,1,1,0)
#'ehour <- c(80, 81, 82, 83, 84, 85, 84, 83, 82, 81, 80)
#'div = 2
#'cs <- cumsumseq(gs, ehour, div = 2)
#'
cumsumseq <- function(gs, ehour, div) {
  sel <- which(gs < 1)
  sq <- 1
  if (length(sel) > 1) {
    for (i in 2:length(sel))
      sq[i] <- ifelse(sel[i] == (sel[i-1] + 1), 0 ,1)
  }
  sel2 <- which(sq == 1)
  sel2 <- sel[sel2]
  for (i in 1:length(sel2)) {
    mx <- ifelse(i < length(sel2), sel2[i+1], length(ehour))
    es <- cumsum(ehour[sel2[i]:mx]) * 12 * div / length(ehour)
    it <- which(es<100) + sel2[i] - 1
    gs[it] <- 1
  }
  gs
}

#'warna
#'@description Prints a warning message if data span more than one year and
#'indicates method followed.
#'
#'@return If true, returns warning message, "Data spans more than one year.
#'Data aggregated by unique month irrespective of year and one value returned".
#'@export
#'
warna <- function() {
  warning ("Data spans more than one year. Data aggregated by unique month
           irrespective of year and one value returned")
}

#'warnb
#'@description Prints a warning message if data span more than one year and
#'indicates method followed.
#'
#'@return If true, returns warning message, "Data spans more than one year.
#'Calculations performed on all data and single value returned".
#'@export
#'
warnb <- function() {
  warning ("Data spans more than one year. Calculations performed on all data
           and single value returned")
}
