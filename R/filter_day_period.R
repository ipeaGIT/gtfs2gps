#' @title Filter GTFS data within a period of the day
#' 
#' @description Updates a GTFS feed filtering only the routes, shapes, trips, stops,
#'  agencies and services that are active within a given period of the day.
#' 
#' @param gtfs A GTFS data.
#' @param period_start A string of type "hh:mm" indicating start of the period (defaults to "00:00:01")
#' @param period_end A string of type "hh:mm" indicating the end of the period (defaults to "23:59:59")
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' # read gtfs data
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' 
#' # filter gtfs data
#' poa_f <- filter_day_period(poa, period_start = "10:00", period_end = "10:20")
filter_day_period <- function(gtfs, period_start = "00:00:01", period_end = "23:59:59"){
  gtfs <- data.table::copy(gtfs)
  
  if(is.na(data.table::as.ITime(period_start))){ stop("Error: Invalid period_start input") }
  if(is.na(data.table::as.ITime(period_end))){ stop("Error: Invalid period_end input") }

  period_start <- gtfstools:::string_to_seconds(period_start)
  period_end <- gtfstools:::string_to_seconds(period_end)
  
  gtfs$stop_times[, departure_time := gtfstools:::string_to_seconds(departure_time)]
  gtfs$stop_times[, arrival_time := gtfstools:::string_to_seconds(arrival_time)]

  if(!is.null(gtfs$frequencies)){
    gtfs$frequencies[, start_time := gtfstools:::string_to_seconds(start_time)]
    gtfs$frequencies[, end_time := gtfstools:::string_to_seconds(end_time)]
  }
  
  # 1) filter stop times
  gtfs$stop_times <- gtfs$stop_times[ data.table::between(departure_time, period_start, period_end), ]
  
  # Update frequencies
  if(test_gtfs_freq(gtfs) == "frequency"){
      gtfs$frequencies <- gtfs$frequencies[start_time >= period_start & end_time <= period_end]
  }

  # Remaining unique stops and trips
  unique_stops <- unique(gtfs$stop_times$stop_id)
  unique_trips <- unique(gtfs$stop_times$trip_id)
  
  # 2) filter STOPS and TRIPS
  gtfs$stops <- gtfs$stops[ stop_id %chin% unique_stops ]
  gtfs$trips <- gtfs$trips[ trip_id %chin% unique_trips ]
  
  # unique values
  unique_routes <- unique(gtfs$trips$route_id)
  unique_shapes <- unique(gtfs$trips$shape_id)
  unique_services <- unique(gtfs$trips$service_id)
  
  # 3) filter ROUTES and SHAPES and SERVICES
  gtfs$routes <- gtfs$routes[ route_id %chin% unique_routes ]
  gtfs$shapes <- gtfs$shapes[ shape_id %chin% unique_shapes ]
  gtfs$calendar <- gtfs$calendar[ service_id %chin% unique_services ]
  
  # 4) filter AGENCY
  if(!is.null(gtfs$agency) && !is.null(unique(gtfs$routes$agency_id)))
    gtfs$agency <- gtfs$agency[ agency_id %chin% unique(gtfs$routes$agency_id) ]

  gtfs$stop_times[, departure_time := gtfstools:::seconds_to_string(departure_time)]
  gtfs$stop_times[, arrival_time := gtfstools:::seconds_to_string(arrival_time)]
  
  if(!is.null(gtfs$frequencies)){
    gtfs$frequencies[, start_time := gtfstools:::seconds_to_string(start_time)]
    gtfs$frequencies[, end_time := gtfstools:::seconds_to_string(end_time)]
  }
  
  # return fun output
  return(gtfs)
}
