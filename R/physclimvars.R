#' events: determines the number of events
#'
#' @description `events` is used to calculate the total number of events (e.g. number of rainfall events).
#' @param x a vector with 0, indicating non-event (e.g. no rainfall), and any other value indicating an event.
#'
#' @return an integer representing the total number of events.
#' @export
#'
#' @examples
#' prec <- sample(rep(0:10,10))
#' events(prec)
#'
events <- function(x) {
  x[x>0] <- 1
  x[x<0] <- 0
  x<-c(x,0)
  length(which(diff(x) == -1))
}
#' gssm: Soil moisture content during the growing season
#'
#' @description `gssm` calculates mean soil moisture content during the growing season period.
#'
#' @param gseason a three-dimensional array of binary values indicating growing conditions (1) = yes, (0) = no.
#' @param soilm a three-dimensional array of fractional soil moisture values.
#'
#' @return a matrix of mean soil moisture values during the growing season.
#' @export
#'
#' @seealso the [gseason()] function can be used to create an array of growing conditions (1) = yes, (0) = no accounting for temperature, precipitation and daylight hours.
#'
#' @examples
#' require(microclima)
#' tme <- as.POSIXlt(c(0:1459) * 3600 * 6, origin = "2010-01-01 00:00", tz = "GMT")
#' gs <- gseason_day(tme, 6, 21)
#' gseason <- array(gs, dim=c(1, 1, 1460))
#' soilm <- array(runif(1460, 0, 100), dim= c(1,1,1460))
#' gssm <- gssm(gseason, soilm)
#'
gssm <- function(gseason, soilm) {
  soilm[is.na(soilm)==TRUE]<-0
  ym <- gseason * soilm
  ysoil <- apply(ym, c(1,2), mean, na.rm=T)
  ysoil
}

#' gsl: Growing season length
#' @description `gsl` calculates the length of the growing season in decimal days.
#'
#' @param gseason a three dimensional array of binary values (1 = growing season, 0 = not growing season).
#' @param year calander year.
#'
#' @return a matrix of values of growing season length in decimal days.
#' @export
#'
#' @seealso the `gseason` function can be used to create an array of growing conditions (1) = yes, (0) = no accounting for temperature, precipitation and daylight hours.
#'
#' @examples
#' tme <- as.POSIXlt(c(0:1459) * 3600 * 6, origin = "2010-01-01 00:00", tz = "GMT")
#' gs <- gseason_day(tme, 6, 21)
#' gsa <- array(gs, dim=c(1, 1, 1460))
#' gsl <- gsl(gsa, 2010)
#'
gsl <- function(gseason, year) {
  lpfun <- function(year) {
    diy <- ifelse(year%%4 == 0, 366, 365)
    if (year%%100 == 0 & year%%400 != 0) diy<-365
    diy
  }
  diy<-lpfun(year)
  rid <- dim(gseason)[3] / diy
  gsl <- apply(gseason,c(1,2),sum,na.rm=T) / rid
  gsl
}
#' gsmax: Maximum temperature during the growing season
#'
#' @description Calculates maximum air temperature during the growing season.
#'
#' @param temp a three dimensional array of temperature values.
#' @param gseason a three dimensional array of binary values (1 = growing season, 0 = not growing season).
#'
#' @return a matrix of values of maximum growing season temperatures.
#' @export
#'
#' @seealso the [gseason()] function can be used to create an array of growing conditions (1) = yes, (0) = no accounting for temperature, precipitation and daylight hours.
#'
#' @examples
#' temp <- array(10 * sin(c(0:1459) / (pi * 150)) + rnorm(1460), dim=c(73,144,1460))
#' tme <- tmecreate(2010, 6)
#' gs <- gseason_day(tme, 6, 21)
#' gseason <- array(gs, dim=c(73, 144, 1460))
#' maxgst <- gsmax(temp, gseason)
gsmax <- function(temp, gseason) {
  gseason[gseason == 0] <- NA
  gstemp<-temp*gseason
  gstemp[is.na(gstemp)==TRUE] <- 0
  gsmax<-apply(gstemp, c(1,2), max, na.rm=TRUE)
  gsmax
}
#' mst: Mean summer temperature
#'
#' @description `mst` calculates the mean temperature during summer, accounting for differences in the timing of the summer period for the northern and southern hemispheres.
#'
#' @param temp a three dimensional array of temperature values (deg C).
#' @param year calendar year.
#' @param startday assumed day of year of start of summer in northen hemisphere in non-leap year.
#' @param endday assumed end of summer. Defaults are 1st June to 31st Aug.
#' @param r a raster of same extent as temp coded as 1 for northern hemisphere and 0 for southern hemisphere.
#'
#' @return a matrix of mean summer temperature values.
#' @export
#'
#' @importFrom raster getValues
#'
#' @details
#' Seasons are flipped in the southern hemisphere. I.e. 1st June (day 152) = day 152+365/2+0.5 = 334 = 1st Dec.
#' in leap years, 1 day added. Startday and endday should be for northern hemisphere, and are calculated for southern hemisphere within the function.
#'
#' @examples
#' temp <- array(10 * sin(c(0:1459) / (pi * 150)) + rnorm(1460), dim=c(73,144,1460))
#' m <- matrix(1, 73, 144)
#' r <- raster(m, crs="+init=epsg:4326")
#' extent(r) <- c(-1.25, 358.75, -91.25, 91.25)
#' enorth<-extent(-1.25,358.75,0,91.25)
#' esouth<-extent(-1.25,358.75,-91.25,0)
#' rn<-crop(r,enorth) * 0 + 1
#' rs<-crop(r,esouth) * 0
#' r<-mosaic(rn,rs,fun=mean)
#' mst(temp, 2010, startday = 152, endday = 243, r)
#'
mst <- function(temp, year, startday = 152, endday = 243, r) {
  lpfun <- function(year) {
    diy <- ifelse(year%%4 == 0, 366, 365)
    if (year%%100 == 0 & year%%400 != 0) diy<-365
    diy
  }
  diy<-lpfun(year)
  startday <- ifelse(diy>365,startday+1,startday)
  startsth <- startday +  floor(diy/2)
  endday <- ifelse(diy>365,endday+1,endday)
  endsth <- (endday + floor(diy/2))%%diy
  rid <- dim(temp)[3] / diy
  sn <- (startday - 1) * rid + 1
  ss <- (startsth - 1) * rid + 1
  en <- endday * rid
  es <- endsth * rid
  rcs <- en-sn+1
  mn<-getValues(r,format="matrix")
  ms <- mn+1
  ms[ms==2] <-0
  tnth<-temp[,,sn:en]
  tsth1 <- temp[,,ss:(dim(temp)[3])]
  tsth2 <- temp[,,1:es]
  tnorth <- (apply(tnth,c(1,2),sum,na.rm=T)/rcs)*mn
  tsouth <- ((apply(tsth1,c(1,2),sum,na.rm=T) + apply(tsth2,c(1,2),sum,na.rm=T))
             / rcs) * ms
  tsummer <- tnorth + tsouth
  tsummer
}
#' ssm: Summer soil moisture content
#'
#' @description `ssm` calculates average soil moisture content over the summer period.
#'
#' @param soilm a three dimensional array of soil moisture values for one year.
#' @param year calendar year.
#' @param startday assumed day of year of start of summer in northen hemisphere in non-leap year.
#' @param endday assumed end of summer. Defaults are 1st June to 31st Aug.
#' @param r a raster of same extent as temp coded as 1 for northern hemisphere and 0 for southern hemisphere.
#'
#' @importFrom raster getValues
#'
#' @return a matrix of mean summer soil moisture values.
#' @export
#'
#' @details Seasons are flipped in southern hemisphere. I.e. 1st June (day 152) = day 152+365/2+0.5 = 334 = 1st Dec.
#' in leap years, 1 day added. Startday and endday should be for northern hemisphere, and are calculated for southern hemisphere within the function.
#'
#' @examples
#' soilm <- array(runif(1460, 0, 65), dim= c(73,144,1460))
#' m <- matrix(1, 73, 144)
#' r <- raster(m, crs="+init=epsg:4326")
#' extent(r) <- c(-1.25, 358.75, -91.25, 91.25)
#' enorth<-extent(-1.25,358.75,0,91.25)
#' esouth<-extent(-1.25,358.75,-91.25,0)
#' rn<-crop(r,enorth) * 0 + 1
#' rs<-crop(r,esouth) * 0
#' r<-mosaic(rn,rs,fun=mean)
#' ssm(soilm, 2010, startday = 152, endday = 243, r)
#'
ssm <- function(soilm, year, startday = 152, endday = 243, r) {
  lpfun <- function(year) {
    diy <- ifelse(year%%4 == 0, 366, 365)
    if (year%%100 == 0 & year%%400 != 0) diy<-365
    diy
  }
  diy<-lpfun(year)
  startday <- ifelse(diy>365,startday+1,startday)
  startsth <- startday +  floor(diy/2)
  endday <- ifelse(diy>365,endday+1,endday)
  endsth <- (endday + floor(diy/2))%%diy
  rid <- dim(soilm)[3] / diy
  sn <- (startday - 1) * rid + 1
  ss <- (startsth - 1) * rid + 1
  en <- endday * rid
  es <- endsth * rid
  rcs <- en-sn+1
  mn<-getValues(r,format="matrix")
  ms <- mn+1
  ms[ms==2] <-0
  pnth<-soilm[,,sn:en]
  psth1 <- soilm[,,ss:(dim(soilm)[3])]
  psth2 <- soilm[,,1:es]
  smnorth <- apply(pnth,c(1,2),mean,na.rm=T)*mn
  smsouth <- (apply(psth1,c(1,2),mean,na.rm=T)) + (apply(psth2,c(1,2),mean,na.rm=T)) * ms
  smsummer <- smnorth + smsouth
  smsummer
}
#' gseason_soilmoist: Growing season soil moisture content
#'
#' @description `gseason_soilmoist` calculates the average soil moisture content during the growing season over specified years.
#'
#' @param startyear earliest calender year to be considered in calculations.
#' @param endyear latest calender year to be considered in calculations.
#' @param ga array of growing binary values indicating growing (1) or non-growing (0) season.
#' @param gs array of growing binary values indicating growing (1) or non-growing (0) season for one year.
#' @param sm array of soil moisture values for one year.
#'
#' @import raster
#' @import ncdf4
#'
#' @return A matrix of mean growing season soil moisture values over specified years.
#' @export
#'
#' @details
#'
#' @seealso Requires function [gssm()] to be loaded.
#' @seealso the [gseason()] function can be used to create an array of growing conditions (1) = yes, (0) = no accounting for temperature, precipitation and daylight hours.
#' @seealso [mtoraster()]
#' @seealso Requires that function [gssm()] is also loaded.
#'
#' @examples
#' require(microclima)
#' tme <- as.POSIXlt(c(0:1459) * 3600 * 6, origin = "2010-01-01 00:00", tz = "GMT")
#' gs <- gseason_day(tme, 6, 21)
#' gseason <- matrix(gs, dim=c(1, 1, 1460))
#' soilm <- array(runif(1460, 0, 100), dim= c(1,1,1460))
#' gseason_soilmoist(2010, 2010, gs, soilm)
#'
gseason_soilmoist <- function(startyear, endyear, gs, sm){
  dim3 <- endyear - startyear + 1
  ysoil <- array(NA, dim = c(dim(sm)[1:2], dim3))
  i <- 1
  for (year in startyear:endyear) {
    print(year)
    ysoil[,,i] <- gssm(gs, sm)
    i <- i +1
  }
  soilm <- apply(ysoil, c(1,2), mean, na.rm=T)
  soilm
}
#'gseastemp: Mean temperature during the growing season
#'
#' @description `gseastemp` calculates the mean air temperature during the growing season over specified years.
#'
#' @param startyear earliest calender year to be considered in calculations.
#' @param endyear latest calender year to be considered in calculations.
#' @param temp array of temperature values for one year.
#' @param gseason array of binary values indicating growing (1) or non-growing (0) season for one year.
#'
#' @import raster
#' @import ncdf4
#'
#' @return A matrix of mean growing season temperature values over the specified years.
#' @export
#'
#' @details details to be added
#'
#' @seealso the [gseason()] function can be used to create an array of growing conditions (1) = yes, (0) = no accounting for temperature, precipitation and daylight hours.
#' @seealso Requires function [gst()] to be loaded.
#' @seealso [mtoraster()] to convert output matrix to a raster.
#' @seealso [nctarray()] to create an array from nc file.
#'
#'
gseastemp <- function(startyear, endyear, temp, gseason) {
  dim3 <- endyear - startyear + 1
  styear <- array(NA, dim = c(dim(temp)[1:2], dim3))
  i <- 1
  for (year in startyear:endyear) {
    print(year)
    styear[,,i] <- gst(temp, gseason)
    i <- i+1
  }
  gstemp<-apply(styear,c(1,2),mean,na.rm=T)
  gstemp
}
#'gseasonprecip: Total precipitation during the growing season
#'
#' @description `gseasonprecip` calculates the total precipitation during the growing season.
#'
#' @param startyear earliest calender year to be considered in calculations.
#' @param endyear latest calender year to be considered in calculations.
#' @param precip 3-dimensional array of precipitation values.
#' @param gseason 3-dimensional array of values indicating growing season (1) or non-growing season (0).
#'
#' @import raster
#' @import ncdf4
#'
#' @return A matrix of mean total precipitation values during the growing season over the specified years.
#' @export
#'
#' @details details to be added
#'
#' @seealso Requires function `gsp` to be loaded.
#' @seealso the `gseason` function can be used to create an array of growing conditions (1) = yes, (0) = no accounting for temperature, precipitation and daylight hours.
#' @seealso [mtoraster()] to convert output matrix to a raster.
#' @seealso [nctarray()] to create an array from an nc file.
#'
gseasprec <- function(startyear, endyear, precip, gseason) {
  dim3 <- endyear - startyear + 1
  styear <- array(NA, dim = c(dim(precip)[1:2], dim3))
  i <- 1
  for (year in startyear:endyear) {
    print(year)
    styear[,,i] <- gsp(prec, gseason)
    i <- i+1
  }
  gsprec<-apply(styear,c(1,2),mean,na.rm=T)
  gsprec
}

#' gseasonlength: Growing season length
#'
#'@description `gseasonlength` calculates the average length of the growing season period (in days) across specified years.
#'
#' @param startyear earliest calender year to be considered in calculations.
#' @param endyear latest calender year to be considered in calculations.
#' @param gseason a 3-dimensional array of growing season values for each year (1 = growing season, 0 = non-growing season).
#'
#' @import raster
#' @import ncdf4
#'
#' @return A matrix of mean growing season length (number of days) over specified years.
#' @export
#'
#' @details Some details required here
#'
#' @seealso the [gseason()] function can be used to create an array of growing conditions (1) = yes, (0) = no accounting for temperature, precipitation and daylight hours.
#' @seealso [mtoraster()] to convert a matrix to a raster object.
#' @seealso [nctarray()] to create array from an nc file.
#' @seealso Requires function `gsl` to be loaded.
#'
gseasonlength <- function(startyear, endyear, gseason)  {
  dim3 <- endyear - startyear + 1
  styear <- array(NA, dim = c(dim(gseason)[1:2], dim3))
  i <- 1
  for (year in startyear:endyear) {
    print(year)
    styear[,,1] <- gsl(gseason, year)
    i <- i +1
  }
  gsl <- apply(styear,c(1,2),mean,na.rm=T)
  gsl
}
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
#' summertemp: Mean summer temperature
#'
#' @description `summertemp` calculates mean summer temperature across specified time period (years).
#'
#' @param startyear the earliest calendar year (AD) to be considered in the
#' calculation.
#' @param endyear the latest calendar year (AD) to be considered in the
#' calculation.
#' @param tempnc full path name of nc file containing temperature values for each year and with data extent: -1.25, 358.75, -91.25, 91.25 when converted to raster format.
#' @param startday Indicates assumed day of year of start of summer in northern
#' hemisphere in non-leap year. Default, 1st June.
#' @param endday Indicates assumed day of year of end of summer in northern
#' hemisphere in non-leap year. Default, 31st August.
#'
#' @import raster
#' @import ncdf4
#'
#' @return a matrix of mean summer temperature values over the specified years.
#' @export
#'
#' @details Function has been designed to run with raster data of dimensions 73 x 144.
#' Seasons are flipped in southern hemisphere. I.e. 1st June (day 152) = day 152+365/2+0.5 = 334 = 1st Dec.
#' in leap years, 1 day added. Startday and endday should be for northern hemisphere, and are calculated for southern hemisphere within the function.
#'
#' @seealso [mtoraster()]
#' @seealso [nctarray()] to create array of temperature values from an nc file.
#' @seealso Requires function [mst()] to be loaded.
summertemp <- function(startyear, endyear, tempnc, startday = 152, endday = 243) {
  dim3 <- endyear - startyear + 1
  styear <- array(NA, dim = c(73, 144, dim3))
  i <- 1
  r<-raster(tempnc)
  enorth<-extent(-1.25,358.75,0,91.25)
  esouth<-extent(-1.25,358.75,-91.25,0)
  rn<-crop(r,enorth) * 0 + 1
  rs<-crop(r,esouth) * 0
  r<-mosaic(rn,rs,fun=mean)
  for (year in startyear:endyear) {
    print(year)
    temp <- nctoarray(tempnc)
    styear[,,i] <- mst(temp, year, startday = startday, endday = endday, r)
    i <- i +1
  }
  sumtemp<-apply(styear,c(1,2),mean,na.rm=T)
  sumtemp
}
#' summersoilmoist: Soil moisture content during summer
#'
#' @description Calculates mean soil moisture content during the 6-month summer period.
#'
#' @param startyear earliest calender year to be considered in calculations.
#' @param endyear latest calender year to be considered in calculations.
#' @param soilnc full path name of nc file containing soil moisture values for each year and with data extent: -1.25, 358.75, -91.25, 91.25 when converted to raster format.
#' @param fi nc file containing soil moisture values for one year.
#' @param startday Indicates assumed day of year of start of summer in northern
#' hemisphere in non-leap year. Default, 1st June.
#' @param endday indicates assumed day of year of end of summer in northern
#' hemisphere in non-leap year. Default, 31st August.
#'
#' @import raster
#' @import ncdf4
#'
#' @return a matrix of mean summer soil moisture content values over the specified years.
#' @export
#'
#' @details Function has been designed to run with raster data of dimensions 73 x 144.
#' Seasons are flipped in southern hemisphere. I.e. 1st June (day 152) = day 152+365/2+0.5 = 334 = 1st Dec.
#' in leap years, 1 day added. Startday and endday should be for northern hemisphere, and are calculated for southern hemisphere within the function.
#'
#' @seealso [mtoraster()]
#' @seealso [nctoarray()] to create array of temperature values from an nc file.
#' @seealso Requires function [ssm()] to be loaded.
#'
summersoilmoist <- function(startyear, endyear, soilnc, fi, startday = 152, endday = 243) {
  dim3 <- endyear - startyear + 1
  styear <- array(NA, dim = c(73, 144, dim3))
  r<-raster(soilnc)
  enorth<-extent(-1.25,358.75,0,91.25)
  esouth<-extent(-1.25,358.75,-91.25,0)
  rn<-crop(r,enorth) * 0 + 1
  rs<-crop(r,esouth) * 0
  r<-mosaic(rn,rs,fun=mean)
  i<-1
  for (year in startyear:endyear) {
    print(year)
    soilm <- nctoarray(fi)
    soilm[soilm<0] <- 0
    soilm[is.na(soilm)==TRUE]<-0
    styear[,,i] <- ssm(soilm, year, startday = startday, endday = endday, r)
    i <- i +1
  }
  meanmoist<-apply(styear,c(1,2),mean,na.rm=T)
  meanmoist
}
