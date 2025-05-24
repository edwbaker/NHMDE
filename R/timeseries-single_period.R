#' Find extreme values from a single period
#'
#' This function finds the extreme value (maximum or minimum) from a single
#' period of data.
#' @param data A vector of numeric values representing the data.
#' @param mode A character string indicating whether to find the "max" or "min"
#'   extreme value. Default is "max".
#' @return The index of the extreme value within the data vector.
#' @importFrom stats median
#' @export
sp_extreme <- function(data, mode="max") {
  skmm <- ts_kernel_min_max(data, n=60, mode=mode)
  medp <- median(which(skmm == max(skmm)))
  return(medp)
}
