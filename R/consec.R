#' consec: Calculate consecutive measurements below or above a specified
#' threshold.
#'
#' @description `consec` is used to find consecutive values above or below
#' a user-defined threshold.
#'
#' @param var a vector of values for which a threshold is to be applied.
#' @param threshold a value with which to threshold the data in `var`.
#' @param below a logical indicating whether values above or below the threshold
#' are to be considered. Default = TRUE.
#'
#' @return a single numeric value of the longest run of measurements that adhere
#' to the threshold criteria.
#' @export
#'
#' @details ???
#'
#' @examples
#' # calculate waterlogged hours
#' set.seed(100)
#' rain <- rgamma(365,4) * rbinom(365,1,0.3)
#' evap <- rep(1.15,365)
#' cn <- 90
#' s_min <- 0.074
#' s_max <- 0.422
#' s_depth <- 0.5
#' sm <- soil_moisture(rain, evap, cn)
#' wlh <- consec(sm, threshold = 0.422, below = FALSE)

consec <- function(x, threshold, below = TRUE) {
  if (below) {
    y <- ifelse(x < threshold,1,0)
  } else {
    y <- ifelse(x > threshold,1,0)
  }
  y2 <- rle(y)
  sel <- which(y2$values == 1)
  max(y2$lengths[sel])
}
