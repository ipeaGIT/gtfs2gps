library(gtfs2gps)
poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))





filter_perio <- function( start = "10:00", end = "12:00"){
  
  

# 1) filter stop times
  poa$stop_times <- poa$stop_times[ between(departure_time, as.ITime(start), as.ITime(end)) , ]
  
  # unique stops and trips
  unique_stops <- unique(poa$stop_times$stop_id)
  unique_trips <- unique(poa$stop_times$trip_id)
  
  # update STOPS and TRIPS
  poa$stops <- poa$stops[ stop_id %in% stop_id ]
  poa$trips <- poa$trips[ trip_id %in% unique_trips ]
  
  # unique values
  unique_routes <- unique(poa$trips$route_id)
  unique_shapes <- unique(poa$trips$shape_id)
  unique_services <- unique(poa$trips$service_id)
  
  
  # update ROUTES and SHAPES and SERVICES
  poa$routes <- poa$routes[ route_id %in% unique_routes ]
  poa$shapes <- poa$shapes[ shape_id %in% unique_shapes ]
  poa$calendar <- poa$calendar[ service_id %in% unique_services ]
  
  # update AGENCY
  poa$agency <- poa$agency[ agency_id %in% unique(poa$routes$agency_id),]

    }