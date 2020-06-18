#' summerprecip: Calculate summer precipitation.
#'
#' @description `summerprecip` calculates the total amount of precipitation
#' falling in summer.
#'
#' @param start_year earliest calender year to be considered in calculations.
#' @param end_year latest calender year to be considered in calculations.
#' @param precipnc full path name of nc file containing precipitation values
#' and with data extent: -1.25, 358.75, -91.25, 91.25 when converted to raster
#' format.
#' @param start_day assumed day of year of start of summer in northern
#' hemisphere in non-leap year* (default 1st June).
#' @param end_day assumed day of year of end of summer in northern hemisphere
#' in non-leap year* (default 31st Aug).
#'
#' @import raster
#' @import ncdf4
#'
#' @return A matrix of mean growing degree days above 10 degrees Celcius during
#' the growing season over the specified years.
#' @export
#'
#' @details Function has been designed to run with raster data of dimensions 73 x 144.
#' Seasons are flipped in southern hemisphere. I.e. 1st June (day 152) = day
#' 152+365/2+0.5 = 334 = 1st Dec. In leap years, 1 day added. `start_day` and
#' `end_day` should be for northern hemisphere, and are calculated for southern
#' hemisphere within the function.
#'
#' @seealso Requires function [tsp()] to be loaded.
#' @seealso [mtoraster()] to convert output matrix to a raster.
#' @seealso [nctarray()] to create an array from an nc file.
#'
summerprecip <- function(start_year, end_year, precipnc, start_day = 152, end_day = 243) {
  dim3 <- end_year - start_year + 1
  styear <- array(NA, dim = c(73, 144, dim3))
  r<-raster(precipnc)
  enorth<-extent(-1.25,358.75,0,91.25)
  esouth<-extent(-1.25,358.75,-91.25,0)
  rn<-crop(r,enorth) * 0 + 1
  rs<-crop(r,esouth) * 0
  r<-mosaic(rn,rs,fun=mean)
  i<-1
  for (year in start_year:end_year) {
    print(year)
    prec <- nctoarray(precipnc)
    prec[prec<0] <- 0
    styear[,,i] <- tsp(prec, year, start_day = start_day, end_day = end_day, r)
    i <- i +1
  }
  sumprec<-apply(styear,c(1,2),mean,na.rm=T)
  sumprec
}
