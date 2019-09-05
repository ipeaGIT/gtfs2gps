
utils::globalVariables(c(".", "%>%", ":="))
.onLoad = function(lib, pkg) {
  # Use GForce Optimisations in data.table operations
  # details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
  options(datatable.optimize = Inf)
  
  # Start parallel processing with the future library
  future::plan(future::multiprocess)
  
  # allow for large files in memory during parallel processing
  options(future.globals.maxSize= Inf)
}

#' @importFrom graphics hist par plot
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @useDynLib gtfs2gps, .registration = TRUE
NULL
