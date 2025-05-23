#' Apply maximum kernel to data
#'
#' This function applies a maximum kernel to the data. It uses a
#' Savitzky-Golay filter to smooth the data and then applies a kernel
#' to find local maxima.
#'
#' @param data A numeric vector of data to be processed.
#' @param n An integer specifying the size of the kernel. Default is 60.
#' @param mode A character string specifying the mode of operation (max or min).
#' @return A numeric vector of the same length as the input data.
#' @importFrom pracma savgol
sensors_kernel_min_max <- function(data, n=60, mode="max") {
  if (mode == "min") {
    return(sensors_kernel_min_max(max(data) - data, n = n))
  }
  data <- pracma::savgol(data, fl=n+1, forder=3, dorder=0)
  n <- 60
  l <- length(data)
  v <- rep(0, n/2)
  for (i in ((n/2)+1):(length(data)-(n/2))) {
    w <- 0
    if (data[i-(n/2)] <= data[i] & data[i] >= data[i+(n/2)]) {
      for (p in c((i-(n/2)):(i-1),(i+1):(i+(n/2)))) {
        if (data[p] <= data[i]) {
          w <- w + abs(data[i])
        }
      }
    }
    v <-c(v,w)
  }
  v <- c(v,rep(0,n/2))
  return(v)
}
