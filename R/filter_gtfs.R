
#' @title Remove invalid objects from GTFS data
#' 
#' @description Remove all invalid (and therefore unnecessary) objects from GTFS data. That is,
#' agency-routes relation (agency_id), routes-trips relation (route_id), 
#' trips-shapes relation (shape_id), trips-frequencies relation (trip_id),
#' trips-stop_times relation (trip_id), stop_times-stops relation (stop_id),
#' and trips-calendar relation (service_id),
#' recursively, until GTFS data does not reduce its size anymore.
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @param only_essential Remove only the essential files? The essential files are all but 
#' agency and calendar. Default is TRUE, which means that agency-routes and trips-calendar relations
#' will not be processed as restrictions to remove objects.
#' @return A subset of the input GTFS data. 
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"), remove_invalid = FALSE)
#' 
#' subset <- remove_invalid(poa)
remove_invalid <- function(gtfs_data, only_essential = TRUE){
  newsize <- object.size(gtfs_data)
  size <- newsize + 1
  
  while(newsize < size){
    size <- newsize
    
    # agency-routes relation (agency_id)
    if(!only_essential){
      agency_ids <- intersect(gtfs_data$agency$agency_id, gtfs_data$routes$agency_id)
      
      gtfs_data$agency <- subset(gtfs_data$agency, agency_id %in% agency_ids)
      gtfs_data$routes <- subset(gtfs_data$routes, agency_id %in% agency_ids)
    }
  
    # routes-trips relation (route_id)
    route_ids <- intersect(gtfs_data$routes$route_id, gtfs_data$trips$route_id)
  
    gtfs_data$routes <- subset(gtfs_data$routes, route_id %in% route_ids)
    gtfs_data$trips  <- subset(gtfs_data$trips,  route_id %in% route_ids)
  
    # trips-shapes relation (shape_id)
    shape_ids <- intersect(gtfs_data$trips$shape_id, gtfs_data$shapes$shape_id)
    
    gtfs_data$trips  <- subset(gtfs_data$trips,  shape_id %in% shape_ids)
    gtfs_data$shapes <- subset(gtfs_data$shapes, shape_id %in% shape_ids)
    
    # trips-frequencies relation (trip_id)
    if(test_gtfs_freq(gtfs_data) == 'frequency'){
      trip_ids <- intersect(gtfs_data$trips$trip_id, gtfs_data$frequencies$trip_id)
  
      gtfs_data$trips       <- subset(gtfs_data$trips,       trip_id %in% trip_ids)
      gtfs_data$frequencies <- subset(gtfs_data$frequencies, trip_id %in% trip_ids)
    }
    
    # trips-stop_times relation (trip_id)
    trip_ids <- intersect(gtfs_data$trips$trip_id, gtfs_data$stop_times$trip_id)
    
    gtfs_data$trips      <- subset(gtfs_data$trips,      trip_id %in% trip_ids)
    gtfs_data$stop_times <- subset(gtfs_data$stop_times, trip_id %in% trip_ids)
    
    # stop_times-stops relation (stop_id)
    stop_ids <- intersect(gtfs_data$stop_times$stop_id, gtfs_data$stops$stop_id)
  
    gtfs_data$stop_times <- subset(gtfs_data$stop_times, stop_id %in% stop_ids)
    gtfs_data$stops      <- subset(gtfs_data$stops,      stop_id %in% stop_ids)
  
    # trips-calendar relation (service_id)
    if(!only_essential){
      service_ids <- intersect(gtfs_data$trips$service_id, gtfs_data$calendar$service_id)
    
      gtfs_data$trips    <- subset(gtfs_data$trips,    service_id %in% service_ids)
      gtfs_data$calendar <- subset(gtfs_data$calendar, service_id %in% service_ids)
    }
  
    newsize <- object.size(gtfs_data)
  }

  return(gtfs_data)
}


#' @title Filter GTFS data by shape ids
#' 
#' @description Filter a GTFS data by its shape ids. It also removes the
#' unnecessary trips, stop_times, stops, and routes accordingly.
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @param shape_ids A vector of shape_ids belonging to the shapes of the
#' gtfs_data data. Note that shape_id might be loaded by gtfs2gps::read_gtfs()
#' as a string or a number, depending on the available values.
#' @param remove_invalid Remove all the invalid objects after subsetting the data?
#' The default value is TRUE.
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' 
#' subset <- filter_by_shape_id(poa, "T2-1")
filter_by_shape_id <- function(gtfs_data, shape_ids, remove_invalid = TRUE){
  gtfs_data$shapes <- subset(gtfs_data$shapes, shape_id %in% shape_ids)
  gtfs_data$trips <- subset(gtfs_data$trips, shape_id %in% shape_ids)
  
  if(remove_invalid){gtfs_data <- gtfs2gps::remove_invalid(gtfs_data)}

  return(gtfs_data)
}

#' @title Filter GTFS data by agency ids
#' 
#' @description Filter a GTFS data by its agency ids. It also removes the
#' unnecessary routes, trips, frequencies, stop_times, calendars, shapes, and
#' stops accordingly.
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @param agency_ids A vector of strings belonging to the agencies of the
#' gtfs_data data.
#' @param remove_invalid Remove all the invalid objects after subsetting the data?
#' The default value is TRUE.
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#'
#' result <- filter_by_agency_id(poa, "EPTC")
filter_by_agency_id <- function(gtfs_data, agency_ids, remove_invalid = TRUE){
  gtfs_data$agency <- subset(gtfs_data$agency, agency_id %in% agency_ids)
  gtfs_data$routes <- subset(gtfs_data$routes, agency_id %in% agency_ids)

  if(remove_invalid){gtfs_data <- gtfs2gps::remove_invalid(gtfs_data)}

  return(gtfs_data)
}

#' @title Filter GTFS data using valid stop times
#' 
#' @description Filter a GTFS data read using gtfs2gps::read_gtfs(). It removes stop_times
#' with NA values in arrival_time, departure_time, and arrival_time_hms. It also filters
#' stops and routes accordingly.
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @param remove_invalid Remove all the invalid objects after subsetting the data?
#' The default value is TRUE.
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' 
#' subset <- filter_valid_stop_times(poa)
filter_valid_stop_times <- function(gtfs_data, remove_invalid = TRUE){
  gtfs_data$stop_times <- subset(gtfs_data$stop_times, !is.na(arrival_time) & !is.na(departure_time))

  if(remove_invalid){gtfs_data <- gtfs2gps::remove_invalid(gtfs_data)}

  return(gtfs_data)
}

#' @title Filter GTFS trips operating on week days
#' 
#' @description Filter a GTFS data read using gtfs2gps::read_gtfs(). It removes the
#' trips operating only saturday or sunday.
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @param remove_invalid Remove all the invalid objects after subsetting the data?
#' The default value is TRUE.
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' 
#' subset <- filter_week_days(poa)
filter_week_days <- function(gtfs_data, remove_invalid = TRUE){
    calendar_temp <- subset(gtfs_data$calendar, monday > 0 | tuesday > 0 | wednesday > 0 | thursday > 0 | friday > 0)
    serviceids <- calendar_temp$service_id
    gtfs_data$trips <- subset(gtfs_data$trips, service_id %in% serviceids)
    gtfs_data$calendar[, sunday := 0]
    gtfs_data$calendar[, saturday := 0]
    
    if(remove_invalid){gtfs_data <- gtfs2gps::remove_invalid(gtfs_data)}

    return(gtfs_data)
}

#' @title Filter GTFS trips in order to have one trip per shape_id
#' 
#' @description Filter a GTFS data by keeping only one trip per shape_id.
#' It also removes the unnecessary routes accordingly.
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @param remove_invalid Remove all the invalid objects after subsetting the data?
#' The default value is TRUE.
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' 
#' subset <- filter_single_trip(poa)
filter_single_trip <- function(gtfs_data, remove_invalid = TRUE){
  gtfs_data$trips <- gtfs_data$trips[!duplicated(gtfs_data$trips$shape_id), ]

  if(remove_invalid){gtfs_data <- gtfs2gps::remove_invalid(gtfs_data)}

  return(gtfs_data)
}
