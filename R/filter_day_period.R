#' @title Filter GTFS data within a period of the day
#' 
#' @description Updates a GTFS feed filtering only the routes, shapes, trips, stops,
#'  agencies and services that are active within a given period of the day.
#' 
#' @param gtfs A GTFS data.
#' @param start A string of type "hh:mm" indicating start of the period (defaults to "06:00")
#' @param end A string of type "hh:mm" indicating the end of the period (defaults to "09:00")
#' @export
#' @examples \donttest{
#' library(gtfs2gps)
#'
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#' poa_f <- filter_day_period(poa, period_start = "10:00", period_end = "19:00")
#' }
#' 
filter_day_period <- function(gps, period_start=NULL, period_end=NULL){
  
if(is.null(period_start)){ period_start <- "00:00:01"}
if(is.null(period_end)){ period_end <- "23:59:59"}

if(is.na(as.ITime(period_start))){ stop( paste0("Error: Invalid period_start input") ) }
if(is.na(as.ITime(period_end))){ stop( paste0("Error: Invalid period_end input") ) }
  
  # 1) filter stop times
  gps$stop_times <- gps$stop_times[ data.table::between(departure_time, data.table::as.ITime(period_start), data.table::as.ITime(period_end)), ]
  
  # unique stops and trips
  unique_stops <- unique(gps$stop_times$stop_id)
  unique_trips <- unique(gps$stop_times$trip_id)
  
  # 2) filter STOPS and TRIPS
  gps$stops <- gps$stops[ stop_id %in% stop_id ]
  gps$trips <- gps$trips[ trip_id %in% unique_trips ]
  
  # unique values
  unique_routes <- unique(gps$trips$route_id)
  unique_shapes <- unique(gps$trips$shape_id)
  unique_services <- unique(gps$trips$service_id)
  
  # 3) filter ROUTES and SHAPES and SERVICES
  gps$routes <- gps$routes[ route_id %in% unique_routes ]
  gps$shapes <- gps$shapes[ shape_id %in% unique_shapes ]
  gps$calendar <- gps$calendar[ service_id %in% unique_services ]
  
  # 4) filter AGENCY
  gps$agency <- gps$agency[ agency_id %in% unique(gps$routes$agency_id),]
  
  # return fun output
  return(gps)
}
