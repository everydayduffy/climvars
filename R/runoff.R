#' Adjusts run-off curve number by antecedent soil moisture
.SCN_adjust <- function(scn, sm, Smin, Smax) {
  theta <- (sm - Smin) / (Smax - Smin)
  scn2 <- scn / 100
  scn2 <- ifelse(theta <= 0.25,  0.242536 * scn2 + 0.678166 * scn2^2 + 0.011486, scn2)
  scn2 <- ifelse(theta >= 0.75,  1.61044 * scn2 - 0.69114 * scn2^2 + 0.06730, scn2)
  scn2 <- scn2 * 100
  scn2 <- ifelse(scn == 100, scn, scn2)
  scn2
}

#' Calculates runoff.
#'
#' @description `runoff` calculates direct runoff based on USDA Run-off curve
#' number.
#'
#' @param rain rainfall in mm.
#' @param cn run-off curve number (see details).
#' @param sm soil water fraction.
#' @param s_min residual water content (m^3 / m^3).
#' @param s_max volumetric water content at saturation (m^3 / m^3).
#' @return direct runoff in mm.
#'
#' @details  Applies the United States Department of Agriculture method for
#' calculating direct runoff. Runoff curve numbers can be obtained from here:
#' https://www.wcc.nrcs.usda.gov/ftpref/wntsc/H&H/training/runoff-curve-numbers1.pdf
#' Adjustments for antecedent moisture condition are automatically applied, so
#' the supplied value of `cn` should be for average soil moisture (AMC II).
#' `s_min` and `s_max` values for soil types are given in [soilparams()].
#' @export
#'
#' @examples
#' rain <- c(1:150)
#' ro1 <- runoff(rain, cn = 50, sm = 0.4, s_min = 0.074, s_max = 0.422)
#' ro2 <- runoff(rain, cn = 90, sm = 0.4, s_min = 0.074, s_max = 0.422)
#' plot(ro1 ~ rain, type = "l")
#' plot(ro2 ~ rain, type = "l")
runoff <- function(rain, cn, sm, s_min, s_max) {
  cn2 <- .SCN_adjust(cn, sm, s_min, s_max)
  P <- 0.0393701 * rain
  S <- (1000 / cn2) - 10
  Ia <- 0.2 * S
  Q <- ifelse(P > Ia, (P - Ia)^2 / (P - Ia + S), 0)
  Qmm <- Q / 0.0393701
  Qmm <- ifelse(sm > s_max, rain, Qmm)
  return(Qmm)
}

