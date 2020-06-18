#' tsp: Calculate summer precipitation.
#'
#' @description `tsp` calculates total precipitation during the summer season.
#'
#' @param prec a three dimensional array of precipitation values.
#' @param start_day assumed day of year of start of summer in northern
#' hemisphere in non-leap year. Default is day 152 (1st June).
#' @param end_day assumed day of year of start of summer in northern hemisphere
#' in non-leap year. Default is day 243 (31st August).
#' @param r a raster object of same extent as temp coded as 1 for northern
#' hemisphere and 0 for southern hemisphere.
#'
#' @importFrom raster getValues
#'
#' @return a matrix of total summer precipitation values for northern and
#' southern hemisphere.
#' @export
#'
#' @details Seasons are flipped in the southern hemisphere i.e. 1st June
#' (day 152) = day 152+365/2+0.5 = 334 = 1st Dec. In leap years, 1 day is added.
#'
#' @examples
#' prec <- array(10 * sin(c(0:1459) * (pi / -1400)) + runif(1460, 0, 10) +10,
#' dim=c(73,144,1460))
#' m <- matrix(1, 73, 144)
#' r <- raster(m, crs="+init=epsg:4326")
#' extent(r) <- c(-1.25, 358.75, -91.25, 91.25)
#' enorth <- extent(-1.25,358.75,0,91.25)
#' esouth <- extent(-1.25,358.75,-91.25,0)
#' rn <- crop(r, enorth) * 0 + 1
#' rs <- crop(r, esouth) * 0
#' r <- mosaic(rn, rs, fun=mean)
#' tsp(prec, 2010, start_day = 152, end_day = 243, r)
#'
tsp <- function(prec, year, start_day = 152, end_day = 243, r) {
  lpfun <- function(year) {
    diy <- ifelse(year%%4 == 0, 366, 365)
    if (year%%100 == 0 & year%%400 != 0) diy<-365
    diy
  }
  diy <- lpfun(year)
  start_day <- ifelse(diy > 365, start_day+1, start_day)
  startsth <- start_day +  floor(diy/2)
  end_day <- ifelse(diy > 365, end_day+1, end_day)
  endsth <- (end_day + floor(diy/2))%%diy
  rid <- dim(prec)[3] / diy
  sn <- (start_day - 1) * rid + 1
  ss <- (startsth - 1) * rid + 1
  en <- end_day * rid
  es <- endsth * rid
  rcs <- en-sn+1
  mn <- getValues(r, format="matrix")
  ms <- mn+1
  ms[ms==2] <- 0
  pnth <- prec[,,sn:en]
  psth1 <- prec[,,ss:(dim(prec)[3])]
  psth2 <- prec[,,1:es]
  pnorth <- apply(pnth, c(1,2), sum, na.rm=T)*mn
  psouth <- (apply(psth1, c(1,2), sum, na.rm=T) + apply(psth2, c(1,2), sum,
                                                       na.rm=T)) * ms
  psummer <- pnorth + psouth
  psummer
}
