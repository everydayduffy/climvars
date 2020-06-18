#' mst: Calculate mean summer temperature.
#'
#' @description `mst` calculates the mean temperature during summer, accounting
#' for differences in the timing of the summer period for the northern and
#' southern hemispheres.
#'
#' @param temps a three dimensional array of temperature values (deg C).
#' @param year calendar year.
#' @param start_day assumed day of year of start of summer in northern
#' hemisphere in non-leap year.
#' @param end_day assumed day of year of end of summer. Defaults are 1st June
#' to 31st Aug.
#' @param r a binary raster of same extent as `temps` coded as 1 for northern
#' hemisphere and 0 for southern hemisphere.
#'
#' @return a matrix of mean summer temperature values.
#' @export
#'
#' @importFrom raster getValues
#'
#' @details Seasons are flipped in the southern hemisphere. I.e. 1st June
#' (day 152) = day 152+365/2+0.5 = 334 = 1st December in leap years, 1 day
#' added. `start_day` and `end_day` should be for northern hemisphere, and are
#' calculated for southern hemisphere within the function.
#'
#' @examples
#' temps <- array(10 * sin(c(0:1459) / (pi * 150)) + rnorm(1460), dim=c(73,144,1460))
#' m <- matrix(1, 73, 144)
#' r <- raster(m, crs="+init=epsg:4326")
#' extent(r) <- c(-1.25, 358.75, -91.25, 91.25)
#' enorth <- extent(-1.25,358.75,0,91.25)
#' esouth <- extent(-1.25,358.75,-91.25,0)
#' rn <- crop(r,enorth) * 0 + 1
#' rs <- crop(r,esouth) * 0
#' r <- mosaic(rn,rs,fun=mean)
#' mst(temps, 2010, start_day = 152, end_day = 243, r)
#'
mst <- function(temps, year, start_day = 152, end_day = 243, r) {
  lpfun <- function(year) {
    diy <- ifelse(year%%4 == 0, 366, 365)
    if (year%%100 == 0 & year%%400 != 0) diy <- 365
    return(diy)
  }
  diy <- lpfun(year)
  start_day <- ifelse(diy > 365, start_day+1, start_day)
  startsth <- start_day + floor(diy/2)
  end_day <- ifelse(diy > 365, end_day+1, end_day)
  endsth <- (end_day + floor(diy/2))%%diy
  rid <- dim(temps)[3] / diy
  sn <- (start_day - 1) * rid + 1
  ss <- (startsth - 1) * rid + 1
  en <- end_day * rid
  es <- endsth * rid
  rcs <- en - sn+1
  mn <- getValues(r, format="matrix")
  ms <- mn+1
  ms[ms==2] <- 0
  tnth <- temps[,,sn:en]
  tsth1 <- temps[,,ss:(dim(temps)[3])]
  tsth2 <- temps[,,1:es]
  tnorth <- (apply(tnth, c(1,2), sum,na.rm=T)/rcs)*mn
  tsouth <- ((apply(tsth1, c(1,2), sum, na.rm=T) + apply(tsth2, c(1,2), sum,
                                                         na.rm=T)) / rcs) * ms
  tsummer <- tnorth + tsouth
  return(tsummer)
}
