#' Accumulated Degree-Days
#'
#' This function calculates the accumulated degree days (ADD) based
#' on timed temperature data.
#' @param timestamp A vector of timestamps in POSIXct format.
#' @param temperature A vector of temperature values.
#' @param baseline The baseline temperature for calculating degree days.
#' @param name A character string for naming the plot.
#' @param plot Logical value indicating whether to plot the delta T values.
#' @return The accumulated degree days (ADD) value.
#' @importFrom graphics boxplot
AccDD <- function(timestamp, temperature, baseline=10, name="", plot=F) {
  deltaT <- diff(as.numeric(timestamp))

  if (plot) {
    boxplot(deltaT, main=paste(name," delta T (n=",length(deltaT),")"), horizontal=T)
  }

  deltaT <- c(deltaT, 0)

  AccDS <- sum((temperature-baseline) * deltaT)
  AccDD <- AccDS / (60*60*24)
  return(AccDD)
}
