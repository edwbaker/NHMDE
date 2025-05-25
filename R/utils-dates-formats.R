#' Get a year-day string from a date
#'
#' This function converts a date to a year-day string in the format "YYYY-DDD".
#' @param date A date object.
#' @return A character string in the format "YYYY-DDD".
#' @importFrom lubridate year yday
#' @export
tf_unix_day <- function(date) {
  second(date) <- 0
  minute(date) <- 0
  hour(date) <- 0
  return(floor(as.numeric(date) / 86400))
}

#' Get the number of seconds into that day from a date object
#'
#' This function calculates the number of seconds into the day from a date object.
#' @param date A date object.
#' @return An integer representing the number of seconds into the day.
#' @importFrom lubridate second<- minute<- hour<-
#' @export
tf_day_seconds <- function(date) {
  ts <- as.numeric(date)
  second(date) <- 0
  minute(date) <- 0
  hour(date) <- 0
  return(ts - as.numeric(date))
}
