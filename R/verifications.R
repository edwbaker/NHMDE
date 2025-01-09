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
      data <- .func_call(project, "verification_factor_recode", data)
    }

    for (i in 1:length(.verification_cols())) {
      if (class(data[[i]]) != .verification_cols()[[i]]) {
        if (.verification_cols()[[i]] == "POSIXct") {
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
    "processing_output_id" = "character",
    "identified_name" = "factor",
    "original_tvk" = "factor",
    "sample_id" = "character",
    "survey_id" = "character",
    "verification_id" = "character",
    "created_datetime" = "POSIXct",
    "active" = "factor",
    "verified_by" = "factor",
    "observation_count" = "numeric",
    "taxon_version_key" = "character",
    "level_1_status" = "character",
    "level_2_status" = "character",
    "taxon_hierarchy_species" = "character",
    "taxon_hierarchy_genus" = "character",
    "taxon_hierarchy_family" = "character",
    "taxon_hierarchy_superfamily" = "character",
    "taxon_hierarchy_infraorder" = "character",
    "taxon_hierarchy_order" = "character",
    "taxon_hierarchy_class" = "character",
    "taxon_hierarchy_subphylum" = "character",
    "taxon_hierarchy_phylum"  = "character",
    "taxon_hierarchy_kingdom" = "character",
    "taxon_hierarchy_suborder"  = "character",
    "taxon_hierarchy_super_class" = "character"
  )
  if (is.null(project)) {
    return(cs)
  } else {
    return(c(cs, .func_call(project, "verification_cols")))
  }
}
