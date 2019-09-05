
utils::globalVariables(c(".", "%>%", ":="))
.onLoad = function(lib, pkg) {
  # Use GForce Optimisations in data.table operations
  # details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
  options(datatable.optimize = Inf)
  
  # Start parallel processing with the future library
  future::plan(future::multiprocess)
  
}

#' @importFrom graphics hist par plot
#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @useDynLib gtfs2gps, .registration = TRUE
NULL
