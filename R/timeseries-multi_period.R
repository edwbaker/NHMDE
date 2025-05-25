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
  days <- tf_unix_day(dates)
  de_data <- as.data.frame(cbind(values, dates, days, mins, maxs))

  de_data$maxs <- as.numeric(de_data$maxs)
  de_data$mins <- as.numeric(de_data$mins)
  de_data$days <- days
  de_data$dates <- dates

  u_days <- unique(de_data$days)
  n_days <- length(u_days)

  ret <- data.frame()

  for (i in 1:n_days) {
    u_day <- u_days[i]
    day_data <- de_data[de_data$days == u_day, ]

    max_p <- median(which(day_data$maxs == max(day_data$maxs)))
    ret <- rbind(ret, list(date=day_data$dates[max_p], unix_day=u_day, type="max", value=day_data$values[max_p]))

    min_p <- median(which(day_data$mins == max(day_data$mins)))
    day_min <- day_data$dates[]
    ret <- rbind(ret, list(date=day_data$dates[min_p], unix_day=u_day, type="min", value=day_data$values[min_p]))

    if (breaks) {
      start_of_day <- day_data$dates[[1]]
      second(start_of_day) <- 0
      minute(start_of_day) <- 0
      hour(start_of_day) <- 0
      ret <- rbind(ret, list(date=start_of_day, unix_day=u_day, type="start", value=NA))
    }

  }
  return(ret)
}

#' Stats from daily_extremes()
#'
#' @param daily_extremes A data frame returned by `daily_extremes()`.
#' @return A data frame with statistics for each day, including minimum, maximum,
#'   range, and their respective timestamps.
#' @export
daily_extremes_stats <- function(daily_extremes) {
  u_days <- unique(daily_extremes$unix_day)
  n_days <- length(u_days)

  c_days <- max(u_days) - min(u_days) + 1

  if (c_days > n_days) {
    message(paste(
      "Message: The data span", c_days, "days, but only", n_days, "days with extremes."
    ))
  }

  ret <- data.frame()
  for (i in 1:n_days) {
    u_day <- u_days[i]

    max   <- daily_extremes[daily_extremes$unix_day == u_day & daily_extremes$type=="max", "value"]
    max_t <- daily_extremes[daily_extremes$unix_day == u_day & daily_extremes$type=="max", "date"]
    min   <- daily_extremes[daily_extremes$unix_day == u_day & daily_extremes$type=="min", "value"]
    min_t <- daily_extremes[daily_extremes$unix_day == u_day & daily_extremes$type=="min", "date"]
    ret <- rbind(ret,
                 list(
                   unix_day=u_day,
                   min=min,
                   min_t=min_t,
                   max=max,
                   max_t=max_t,
                   range=max-min,
                   min_p= (min_t - u_day*86400)/86400,
                   max_p= (max_t - u_day*86400)/86400,
                   range_p= (max_t - min_t)/86400
                 ))
  }
  return(ret)
}


#' Remove days with missing data
#'
#' This function removes days from a dataset where the maximum time difference
#' between consecutive timestamps exceeds a specified threshold.
#' @param data A data frame containing a date column.
#' @param datecol The name of the date column in the data frame.
#' @param max_diff The maximum allowed time difference in seconds between
#'   consecutive timestamps. Default is 30 seconds.
#' @return A data frame with the days that have missing data removed.
#' @export
daily_delete_missing <- function(data, datecol, max_diff=30) {
  data$d_d_m_days <- tf_unix_day(data[,datecol])
  u_days <- unique(data$d_d_m_days)
  n_days <- length(u_days)

  rm_days <- c()

  for (i in 1:n_days) {
    i_day <- u_days[i]
    dw <- data[data$d_d_m_days==i_day,]

    start_i_day <- dw[1,datecol]
    second(start_i_day) <- 0
    minute(start_i_day) <- 0
    hour(start_i_day) <- 0

    end_i_day <- dw[1,datecol]
    second(end_i_day) <- 59
    minute(end_i_day) <- 59
    hour(end_i_day) <- 23

    dd <- as.numeric(diff(c(start_i_day, dw[,datecol], end_i_day)))
    if (max(dd) > max_diff) {
      rm_days <- c(rm_days, i_day)
    }
  }

  data <- data[!data$d_d_m_days %in% rm_days,]
  data$d_d_m_days <- NULL

  return(data)
}
