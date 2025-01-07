#' Get verifications data from a file
#'
#' @param file File path
#' @param project Project name
#' @return A data frame with the verifications data
#' @export
verifications_get_data <- function(file, project=NULL) {
  if (!is.null(file)) {
    if (!file.exists(file)) {
      stop("File not found.")
    }
    if (!is.null(project) & !.func_exists(project, "verification_cols")) {
      stop("No function to get data from project.")
    }

    data <- read.csv(file)
    .validate_verification_data(data, project=project)

    if (.func_exists(project, "verification_factor_recode")) {
      data <- .func_call(project, "verificatio_factor_recode", data)
    }

    for (i in 1:length(.cs_data_cols())) {
      if (class(data[[i]]) != .cs_data_cols()[[i]]) {
        if (.cs_data_cols()[[i]] == "POSIXct") {
          data[[i]] <- as.POSIXct(data[[i]], format="%d/%m/%Y %H:%M")
        } else {
          data[[i]] <- as(data[[i]], .cs_data_cols(project)[[i]])
        }
      }
    }

    if (.func_exists(project, "verification_manipulate_cols")) {
      data <- .func_call(project, "verification_manipulate_cols", data)
    }

    return(data)
  }
  stop("File must be specified.")
}

.validate_verification_data <- function(data, project=NULL) {
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.")
  }
  if (!all(names(.verification_cols(project)) %in% colnames(data))) {
    stop("Data does not have expected columns.")
  }
}

.verification_cols <- function(project=NULL) {
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
    return(c(cs, .func_call(project, "verification_cols")))
  }
}
