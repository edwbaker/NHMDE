#' @importFrom utils getFromNamespace
.func_exists <- function(project, func_name) {
  if (is.null(project)) {
    return(FALSE)
  }
  helper_func <- paste0(".",project,"_", func_name)
  tryCatch({
    is.function(getFromNamespace(helper_func, "NHMDE"))
    return(TRUE)
  }, error=function(e) return(FALSE)
  )
}

.func_call <- function(project, func_name, ...) {
  helper_func <- paste0(".",project,"_", func_name)
  do.call(helper_func, list(...))
}

.data_private <- function(project=NULL) {
  if (is.null(project)) {
    return(NULL)
  }
  return(.func_call(project, "data_private"))
}


#' Anonymise data
#'
#' This function anonymises data by hashing private columns defined by helper functions
#' within the package.
#' @param data A data frame to be anonymised.
#' @param project A character string with the name of the project.
#' @param keep_original A logical value indicating whether to keep the original columns.
#' @return A data frame with the private columns hashed as new columns.
#' @export
#' @importFrom openssl md5
anonymise <- function(data, project=NULL, keep_original=FALSE) {
  subs <- .data_private(project)
  for (i in 1:length(subs)) {
    new_col <- names(subs[i])
    if (length(subs[[i]]) == 1) {
      data[, new_col] <- data[, subs[[i]]]
      data[, new_col] <- md5(data[, new_col])
    } else {
      data[, new_col] <- apply( data[ , colnames(data) %in% subs[[i]] ] , 1 , paste , collapse = "_")
      data[, new_col] <- md5(data[, new_col])
    }
    if (!keep_original) {
      data <- data[ , !(colnames(data) %in% subs[[i]])]
    }
  }

  return(data)
}
