
utils::globalVariables(c(".", "%>%", ":="))
.onLoad = function(lib, pkg) {
  # Use GForce Optimisations in data.table operations
  # details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
  options(datatable.optimize = Inf) # nocov
  
  # set number of threads used in data.table to 100% 
  data.table::setDTthreads(percent = 100) # nocov
  
}

#' @importFrom magrittr %>%
#' @importFrom foreach %dopar% foreach
#' @importFrom data.table := %between% fifelse
#' @importFrom stats na.omit
#' @importFrom utils head tail
#' @importFrom stats lag na.omit
#' @useDynLib gtfs2gps, .registration = TRUE
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1") utils::globalVariables(
  c('dist', 'shape_id', 'route_id', 'trip_id', 'stop_id',
    'service_id', 'stop_sequence', 'i.stop_lat', 'i.stop_lon', 'i.stop_id',
    'departure_time', 'arrival_time', 'start_time', 'end_time', 'i.stop_sequence',
    'shape_pt_lon', 'shape_pt_lat', 'id', 'cumdist', 'i.departure_time',
    '.N', 'update_newstoptimes', 'shape_pt_sequence', 'geometry',
    'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday',
    'service_duration', 'headway_secs', 'number_of_departures',
    'cumtime', 'speed', 'lag', 'i'))
