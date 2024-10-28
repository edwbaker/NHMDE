#' @importFrom tidyjson spread_all
flytunes_process_zooniverse <- function(data) {
  metadata <- spread_all(data$metadata)
  annotations <- .flytunes_annotations(data$annotations)
  subject_data <- .flytunes_subject_data(data$subject_data)
  data <- cbind(
    data[, !(colnames(data) %in% c("metadata", "annotations", "subject_data"))],
    metadata[, 2:(length(metadata)-1)],
    annotations[, 2:(length(annotations)-1)],
    subject_data
  )
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
  return(data)
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

  for (i in 1:length(task$value)) {
    if (task$value[[i]] %in% ans) {
      ret[task$value[[i]]] <- TRUE
    }
  }


  return(ret)
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

