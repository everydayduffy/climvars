

#'maxgstemp: Maximum temperature during the growing season
#'
#' @description Calculates the maximum air temperature during the growing season across specified years.
#'
#' @param startyear earliest calender year to be considered in calculations.
#' @param endyear latest calender year to be considered in calculations.
#' @param temp a three-dimensional array of temperature values for one year.
#' @param gs a three-dimensional array of binary values indicating growing (1) or non-growing (0) season for one year.
#'
#' @return A matrix of mean maximum growing season air temperature values over the specified years.
#' @export
#'
#' @import raster
#' @import ncdf4
#'
#' @details some details needed here
#'
#' @seealso [mtoraster()] to convert a matrix to a raster object.
#' @seealso [nctarray()] to create array of temperature values from an nc file.
#' @seealso Requires function [gsmax()] to be loaded.
#'
maxgstemp <- function(startyear, endyear, temp, gs) {
  dim3 <- endyear - startyear + 1
  styear <- array(NA, dim = c(dim(temp)[1:2], dim3))
  i <- 1
  for (year in startyear:endyear) {
    print(year)
    styear[,,i] <- gsmax(temp, gs)
    i <- i+1
  }
  gsmax<-apply(styear,c(1,2),mean,na.rm=T)
  gsmax
}

