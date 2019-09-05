
utils::globalVariables(c(".", "%>%", ":="))
.onLoad = function(lib, pkg) {
  # Use GForce Optimisations in data.table operations
  # details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
  options(datatable.optimize = Inf)
}

#' @importFrom magrittr %>%
#' @importFrom data.table :=
#' @useDynLib gtfs2gps, .registration = TRUE
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(
  c('dist', 'shape_id', 'route_id', 'trip_id', 'stop_id',
    'service_id', 'stop_sequence', 'i.stop_lat', 'i.stop_lon', 'i.stop_id',
    'departure_time', 'arrival_time', 'start_time', 'end_time', 'i.stop_sequence',
    'shape_pt_lon', 'shape_pt_lat', 'id', 'cumdist', 'i.departure_time',
    '.N', 'update_newstoptimes',
    'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'))
