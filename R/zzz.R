
utils::globalVariables(c(".", "%>%", ":="))
.onLoad = function(lib, pkg) {
  # Use GForce Optimisations in data.table operations
  # details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
  options(datatable.optimize = Inf)
}

#' @importFrom graphics hist par plot
#' @importFrom data.table :=
#' @useDynLib gtfs2gps, .registration = TRUE
NULL
