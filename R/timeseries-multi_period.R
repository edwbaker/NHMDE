#' Get the daily extremes from a time series
#'
#' This function calculates the daily maximum and minimum values from a time
#' series of values and their corresponding dates.
#' @param values A numeric vector of values.
#' @param dates A vector of dates corresponding to the values.
#' @param n An integer specifying the number of points to consider for the kernel
#' min/max calculation. Default is 60.
#' @param breaks A logical indicating whether to include the start of the day in the
#' results. Default is FALSE.
#' @return A data frame with the dates of the daily extremes and their types
#' ("max", "min", and optionally "start" if breaks is TRUE).
#' @export
daily_extremes <- function(values, dates, n=60, breaks=FALSE) {
  mins <- ts_kernel_min_max(values, n=n, mode="min")
  maxs <- ts_kernel_min_max(values, n=n, mode="max")
  days <- tf_year_day(dates)
  de_data <- as.data.frame(cbind(values, dates, days, mins, maxs))

  de_data$maxs <- as.numeric(de_data$maxs)
  de_data$mins <- as.numeric(de_data$mins)
  de_data$dates <- dates

  u_days <- unique(de_data$days)
  n_days <- length(u_days)

  ret <- data.frame()

  for (i in 1:n_days) {
    u_day <- u_days[i]
    day_data <- de_data[de_data$days == u_day, ]

    max_p <- median(which(day_data$maxs == max(day_data$maxs)))
    ret <- rbind(ret, list(date=day_data$dates[max_p], type="max", value=day_data$values[max_p]))

    min_p <- median(which(day_data$mins == max(day_data$mins)))
    day_min <- day_data$dates[]
    ret <- rbind(ret, list(date=day_data$dates[min_p], type="min", value=day_data$dates[min_p]))

    if (breaks) {
      start_of_day <- day_data$dates[[1]]
      second(start_of_day) <- 0
      minute(start_of_day) <- 0
      hour(start_of_day) <- 0
      ret <- rbind(ret, list(date=start_of_day, type="start", value=NA))
    }

  }
  return(ret)
}
