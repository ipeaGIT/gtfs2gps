#' @title Filter GTFS data within a period of the day
#' 
#' @description Updates a GTFS feed filtering only the routes, shapes, trips, stops,
#'  agencies and services that are active within a given period of the day.
#' 
#' @param gtfs A GTFS data.
#' @param period_start A string of type "hh:mm" indicating start of the period (defaults to "06:00")
#' @param period_end A string of type "hh:mm" indicating the end of the period (defaults to "09:00")
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' # read gtfs data
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' 
#' # filter gtfs data
#' poa_f <- filter_day_period(poa, period_start = "10:00", period_end = "10:20")
filter_day_period <- function(gtfs, period_start=NULL, period_end=NULL){
  if(is.null(period_start)){ period_start <- "00:00:01"}
  if(is.null(period_end)){ period_end <- "23:59:59"}

  if(is.na(data.table::as.ITime(period_start))){ stop("Error: Invalid period_start input") }
  if(is.na(data.table::as.ITime(period_end))){ stop("Error: Invalid period_end input") }

  # 1) filter stop times
  gtfs$stop_times <- gtfs$stop_times[ data.table::between(departure_time, data.table::as.ITime(period_start), data.table::as.ITime(period_end)), ]
  
  # Update frequencies
  if(test_gtfs_freq(gtfs) == "frequency"){
    
    if((data.table::as.ITime(period_end) - data.table::as.ITime(period_start)) < data.table::as.ITime("01:00")){
      stop("Using a frequency-based GTFS. Please input time period of one hour or longer")
    } else {
      gtfs$frequencies <- gtfs$frequencies[ data.table::as.ITime(start_time) >= data.table::as.ITime(period_start) & data.table::as.ITime(end_time) <= data.table::as.ITime(period_end) ] 
    }
  }
  # as.POSIXct(paste('2020-01-29', '10:45:00')) %>% as.numeric()

  # Remaining unique stops and trips
  unique_stops <- unique(gtfs$stop_times$stop_id)
  unique_trips <- unique(gtfs$stop_times$trip_id)
  
  # 2) filter STOPS and TRIPS
  gtfs$stops <- gtfs$stops[ stop_id %in% unique_stops ]
  gtfs$trips <- gtfs$trips[ trip_id %in% unique_trips ]
  
  # unique values
  unique_routes <- unique(gtfs$trips$route_id)
  unique_shapes <- unique(gtfs$trips$shape_id)
  unique_services <- unique(gtfs$trips$service_id)
  
  # 3) filter ROUTES and SHAPES and SERVICES
  gtfs$routes <- gtfs$routes[ route_id %in% unique_routes ]
  gtfs$shapes <- gtfs$shapes[ shape_id %in% unique_shapes ]
  gtfs$calendar <- gtfs$calendar[ service_id %in% unique_services ]
  
  # 4) filter AGENCY
  gtfs$agency <- gtfs$agency[ agency_id %in% unique(gtfs$routes$agency_id),]
  
  # return fun output
  return(gtfs)
}
