#' load_python_functions
#' @export
load_python_functions <- function(){
  path <- paste(system.file(package="reinforcer"), "gymr.py", sep="/")
  reticulate::source_python(path)
}
