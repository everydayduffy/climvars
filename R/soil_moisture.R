#' Calculates volumetric soil moisture.
#'
#' @description `soil_moisture` calculates volumetric soil water fraction of
#' soil.
#'
#' @param prec a vector of precipitation values (mm).
#' @param evap a vector of evapotranspiration values (mm).
#' @param cn run-off curve number (see details).
#' @param s_depth depth of the soil layer (m).
#' @param s_min residual fraction content (m^3 / m^3).
#' @param s_max volumetric water fraction at saturation (m^3 / m^3).
#' @param smi initial volumetric soil water fraction.
#' @return volumetric water fraction for each time step.
#'
#' @details  Applies a simple bucket model to calculate the volumetric soil
#' water fraction. The run-off curve number controls maximum infiltration rates
#' such that if rainfall exceeds maximum infiltration, surplus water is assumed
#' not to enter. Maximum infiltration is calculated by applying the United
#' States Department of Agriculture method for calculating direct runoff.
#' Runoff curve numbers can be obtained from here:
#' https://www.wcc.nrcs.usda.gov/ftpref/wntsc/H&H/training/runoff-curve-numbers1.pdf
#' Adjustments for antecedent moisture condition are automatically applied, so
#' the supplied value of `cn` should be for average soil moisture (AMC II).
#' `s_min` and `s_max` values for soil types are given in [ecohydrotools::soilparams()].
#' @export
#'
#' @examples
#' set.seed(100)
#' rain <- rgamma(365,4) * rbinom(365,1,0.3)
#' evap <- rep(1.15,365)
#' cn <- 90
#' s_min <- 0.074
#' s_max <- 0.422
#' s_depth <- 0.5
#' sm1 <- soil_moisture(rain, evap, 0.9)
#' sm2 <- soil_moisture(rain, evap, 0.9, 0.1)
#' plot(sm1, type = "l", col = "blue", ylim = c(s_min, s_max),
#'      xlab = "Day", ylab = "Soil moisture")
#' par(new=T)
#' plot(sm2, type = "l", col = "red", ylim = c(s_min, s_max),
#'      xlab = "", ylab = "")
#'
soil_moisture <- function(rain, evap, cn, s_depth = 0.5, s_min = 0.074,
                         s_max = 0.422, smi = (s_max + s_min) / 2) {
  sm <- smi
  for (i in 1:length(rain)) {
    ro <- runoff(rain[i],cn,sm[i],s_min,s_max)
    sm[i+1] <- sm[i] + (rain[i] - ro - evap[i]) / (1000 * s_depth)
    sm[i+1] <- ifelse(sm[i+1] > s_max, s_max, sm[i+1])
    sm[i+1] <- ifelse(sm[i+1] < s_min, s_min, sm[i+1])
  }
  sm <- sm[-1]
  return(sm)
}
