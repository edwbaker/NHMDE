#' Get data from a community science project
#'
#' @param file A character string with the path to the file to read.
#' @param project A character string with the name of the project.
#' @return A data frame with the data.
#' @export
#' @importFrom methods as
#' @importFrom utils read.csv
cs_get_data <- function(file=NULL, project=NULL) {
  if (!is.null(file)) {
    if (!file.exists(file)) {
      stop("File not found.")
    }
    if (!is.null(project) & !.func_exists(project, "data_cols")) {
      stop("No function to get data from project.")
    }

    data <- read.csv(file)
    .validate_cs_data(data, project=project)

    for (i in 1:length(.cs_data_cols())) {
      if (class(data[[i]]) != .cs_data_cols()[[i]]) {
        if (.cs_data_cols()[[i]] == "POSIXct") {
          data[[i]] <- as.POSIXct(data[[i]], format="%d/%m/%Y %H:%M")
        } else {
        data[[i]] <- as(data[[i]], .cs_data_cols(project)[[i]])
        }
      }
    }

    return(data)
  }
  stop("File must be specified.")
}

.cs_data_cols <- function(project=NULL) {
  cs <- list(
    "sample_id" = "character",
    "submission_id" = "character",
    "survey_id" = "character",
    "sample_start_datetime" = "POSIXct",
    "sample_end_datetime" = "POSIXct",
    "submission_datetime" = "POSIXct",
    "first_name" = "character",
    "last_name" = "character",
    "group_name" = "character",
    "sample_observers_type" = "character",
    "sample_observers_number" = "numeric"
  )
  if (is.null(project)) {
    return(cs)
  } else {
    return(c(cs, .func_call(project, "data_cols")))
  }
}

.validate_cs_data <- function(data, project=NULL) {
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.")
  }
  if (!all(names(.cs_data_cols(project)) %in% colnames(data))) {
    stop("Data does not have expected columns.")
  }
}
