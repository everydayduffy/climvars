#' summertemp: Calculate mean summer temperature.
#'
#' @description `summertemp` calculates mean summer temperature across
#' specified time period (years).
#'
#' @param start_year the earliest calendar year (AD) to be considered in the
#' calculation.
#' @param end_year the latest calendar year (AD) to be considered in the
#' calculation.
#' @param tempnc full path name of nc file containing temperature values for
#' each year and with data extent: -1.25, 358.75, -91.25, 91.25 when converted
#' to raster format.
#' @param start_day Indicates assumed day of year of start of summer in northern
#' hemisphere in non-leap year. Default, 1st June.
#' @param end_day Indicates assumed day of year of end of summer in northern
#' hemisphere in non-leap year. Default, 31st August.
#'
#' @import raster
#' @import ncdf4
#'
#' @return a matrix of mean summer temperature values over the specified years.
#' @export
#'
#' @details Function has been designed to run with raster data of dimensions
#' 73 x 144. Seasons are flipped in southern hemisphere. I.e. 1st June
#' (day 152) = day 152+365/2+0.5 = 334 = 1st Dec. In leap years, 1 day added.
#' `start_day` and `end_day` should be for northern hemisphere, and are
#' calculated for southern hemisphere within the function.
#'
#' @seealso [mtoraster()]
#' @seealso [nctarray()] to create array of temperature values from an nc file.
#' @seealso Requires function [mst()] to be loaded.
#'
summertemp <- function(start_year, end_year, tempnc, start_day = 152,
                       end_day = 243) {
  dim3 <- end_year - start_year + 1
  styear <- array(NA, dim = c(73, 144, dim3))
  i <- 1
  r <- raster(tempnc)
  enorth <- extent(-1.25,358.75,0,91.25)
  esouth <- extent(-1.25,358.75,-91.25,0)
  rn <- crop(r,enorth) * 0 + 1
  rs <- crop(r,esouth) * 0
  r <- mosaic(rn,rs,fun=mean)
  for (year in start_year:end_year) {
    print(year)
    temp <- nctoarray(tempnc)
    styear[,,i] <- mst(temp, year, start_day = start_day, end_day = end_day, r)
    i <- i +1
  }
  sumtemp <- apply(styear, c(1,2), mean, na.rm=T)
  return(sumtemp)
}
