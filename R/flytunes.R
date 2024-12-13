#' Process FlyTunes export from Zooniverse
#'
#' @param data A data frame from read.csv of classification csv
#' @return A data frame with processed data
#' @importFrom tidyjson spread_all
#' @export
flytunes_process_zooniverse <- function(data) {
  metadata <- as.data.frame(spread_all(data$metadata))
  annotations <- .flytunes_annotations(data$annotations)
  subject_data <- .flytunes_subject_data(data$subject_data)

  offset <- vector(mode="numeric", length=nrow(data))
  offset[data$workflow_name=="live"] <- 0
  offset[data$workflow_name=="Second batch"] <- 3

  subj_names <- c(colnames(subject_data), "subject.offset")
  subject_data <- cbind(subject_data, offset)
  colnames(subject_data) <- subj_names

  data <- cbind(
    data[, !(colnames(data) %in% c("metadata", "annotations", "subject_data"))],
    metadata[, 2:(length(metadata)-1)],
    annotations,
    subject_data
  )

  return(data)
}

#' @importFrom rjson fromJSON
.flytunes_subject_data <- function(json) {
  if (length(json) >  1) {
    data <- t(sapply(json, .flytunes_subject_data))
    rownames(data) <- NULL
    return(as.data.frame(data))
  }
  task <- rjson::fromJSON(json)[[1]]

  if ("Filename" %in% names(task)){
    task$file <- task$Filename
  }

  file_data <- vector(mode="character", length=length(.flytunes_workflow_cols()))
  names(file_data) <- .flytunes_workflow_cols()
  for (i in seq_along(.flytunes_workflow_cols())) {
    if  (names(file_data)[i] %in% names(task)) {
      file_data[names(file_data)[i]] <- task[names(file_data)[i]]
    }
  }
  if (is.null(task$retired)) {
    task$retired <- vector(mode="character", length=8)
    names(task$retired) <- c("id","workflow_id","classifications_count","created_at",
      "updated_at", "retired_at", "subject_id",  "retirement_reason")
  }
  data <- c(task$retired, file_data)
  names(data) <- c(
    paste0("retired.", names(task$retired)),
    .flytunes_workflow_cols()
  )

  if (data$segment.file == "" & data$segment.length == "" & data$segment.number == "") {
    ss <- sapply(data$file[1], strsplit, "_")
    data$segment.file <- paste(ss[[1]][1], ss[[1]][2], sep="_")
    data$segment.length <- ss[[1]][3]
    data$segment.number <- sub("\\..*", "", ss[[1]][4])
  }
  return(as.data.frame(data))
}

.flytunes_workflow_cols <- function() {
  return(c(
    "file",
    "segment.file",
    "segment.length",
    "segment.number"
  ))
}

#' @importFrom rjson fromJSON
.flytunes_annotations <- function(json) {
  if (length(json) >  1) {
    data <- t(sapply(json, .flytunes_annotations))
    rownames(data) <- NULL
    return(as.data.frame(data))
  }
  task <- rjson::fromJSON(json)[[1]]

  ans <- .flytunes_list_annotations()
  ret <- vector(mode="logical", length=length(ans))
  names(ret) <- ans

  if (length(task$value) > 0) {
    for (i in 1:length(task$value)) {
      if (task$value[[i]] %in% ans) {
        ret[task$value[[i]]] <- TRUE
      }
    }
  }
  return(t(as.data.frame(ret)))
}

.flytunes_list_annotations <- function() {
  return(c(
    "Road vehicles e.g. cars/vans",
    "Other modes of transport e.g. trains/planes",
    "Birds",
    "Insects",
    "Humans e.g. walking/talking",
    "Other/I don't know"
  ))
}

