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
