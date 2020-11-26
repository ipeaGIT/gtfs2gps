
#' @title Remove invalid objects from GTFS data
#' 
#' @description Remove all objects from GTFS data that are not used in all relations
#' that they are required to be. That is,
#' agency-routes relation (agency_id), routes-trips relation (route_id), 
#' trips-shapes relation (shape_id), trips-frequencies relation (trip_id),
#' trips-stop_times relation (trip_id), stop_times-stops relation (stop_id),
#' and trips-calendar relation (service_id),
#' recursively, until GTFS data does not reduce its size anymore. For example,
#' if one agency_id belongs to routes but not to agency will be removed. This might
#' cause one cascade removal of objects in other relations that originally
#' did not have any inconsistency.
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @param only_essential Remove only the essential files? The essential files are all but 
#' agency, calendar, and routes. Default is TRUE, which means that agency-routes,
#' routes-trips, and trips-calendar relations
#' will not be processed as restrictions to remove objects.
#' @param prompt_invalid Show the invalid objects. Default is FALSE.
#' @return A subset of the input GTFS data. 
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' object.size(poa)
#' subset <- remove_invalid(poa)
#' object.size(subset)
remove_invalid <- function(gtfs_data, only_essential = TRUE, prompt_invalid = FALSE){
  newsize <- object.size(gtfs_data)
  size <- newsize + 1
  
  removed <- list(
    agency_ids = c(),
    route_ids = c(),
    shape_ids = c(),
    trip_ids = c(),
    stop_ids = c(),
    service_ids = c()
  )
  
  while(newsize < size){
    size <- newsize
    
    # agency-routes relation (agency_id)
    if(!only_essential && !is.null(gtfs_data$agency) && !is.null(gtfs_data$routes)){
      agency_ids <- intersect(gtfs_data$agency$agency_id, gtfs_data$routes$agency_id)
      removed$agency_ids <- c(removed$agency_ids, setdiff(gtfs_data$agency$agency_id, gtfs_data$routes$agency_id))

      gtfs_data$agency <- subset(gtfs_data$agency, agency_id %in% agency_ids)
      gtfs_data$routes <- subset(gtfs_data$routes, agency_id %in% agency_ids)
    }
  
    # routes-trips relation (route_id)
    if(!only_essential && !is.null(gtfs_data$routes)){
      route_ids <- intersect(gtfs_data$routes$route_id, gtfs_data$trips$route_id)
      removed$route_ids <- c(removed$route_ids, setdiff(gtfs_data$routes$route_id, gtfs_data$trips$route_id))
  
      gtfs_data$routes <- subset(gtfs_data$routes, route_id %in% route_ids)
      gtfs_data$trips  <- subset(gtfs_data$trips,  route_id %in% route_ids)
    }

    # trips-shapes relation (shape_id)
    shape_ids <- intersect(gtfs_data$trips$shape_id, gtfs_data$shapes$shape_id)
    removed$shape_ids <- c(removed$shape_ids, setdiff(gtfs_data$trips$shape_id, gtfs_data$shapes$shape_id))

    gtfs_data$trips  <- subset(gtfs_data$trips,  shape_id %in% shape_ids)
    gtfs_data$shapes <- subset(gtfs_data$shapes, shape_id %in% shape_ids)
    
    # trips-frequencies relation (trip_id)
    if(test_gtfs_freq(gtfs_data) == 'frequency'){
      trip_ids <- intersect(gtfs_data$trips$trip_id, gtfs_data$frequencies$trip_id)
      removed$trip_ids <- c(removed$trip_ids, setdiff(gtfs_data$trips$trip_id, gtfs_data$frequencies$trip_id))

      gtfs_data$trips       <- subset(gtfs_data$trips,       trip_id %chin% trip_ids)
      gtfs_data$frequencies <- subset(gtfs_data$frequencies, trip_id %chin% trip_ids)
    }
    
    # trips-stop_times relation (trip_id)
    trip_ids <- intersect(gtfs_data$trips$trip_id, gtfs_data$stop_times$trip_id)
    removed$trip_ids <- c(removed$trip_ids, setdiff(gtfs_data$trips$trip_id, gtfs_data$stop_times$trip_id))

    gtfs_data$trips      <- subset(gtfs_data$trips,      trip_id %chin% trip_ids)
    gtfs_data$stop_times <- subset(gtfs_data$stop_times, trip_id %chin% trip_ids)
    
    # stop_times-stops relation (stop_id)
    stop_ids <- intersect(gtfs_data$stop_times$stop_id, gtfs_data$stops$stop_id)
    removed$stop_ids <- c(removed$stop_ids, setdiff(gtfs_data$stop_times$stop_id, gtfs_data$stops$stop_id))

    gtfs_data$stop_times <- subset(gtfs_data$stop_times, stop_id %chin% stop_ids)
    gtfs_data$stops      <- subset(gtfs_data$stops,      stop_id %chin% stop_ids)
  
    # trips-calendar relation (service_id)
    if(!only_essential & !is.null(gtfs_data$calendar)){
      service_ids <- intersect(gtfs_data$trips$service_id, gtfs_data$calendar$service_id)
      removed$service_ids <- c(removed$service_ids, setdiff(gtfs_data$trips$service_id, gtfs_data$calendar$service_id))

      gtfs_data$trips    <- subset(gtfs_data$trips,    service_id %chin% service_ids)
      gtfs_data$calendar <- subset(gtfs_data$calendar, service_id %chin% service_ids)
    }

    # trips-calendar_dates relation (service_id)
    if(!only_essential & !is.null(gtfs_data$calendar_dates)){
      service_ids <- intersect(gtfs_data$trips$service_id, gtfs_data$calendar_dates$service_id)
      removed$service_ids <- c(removed$service_ids, setdiff(gtfs_data$trips$service_id, gtfs_data$calendar_dates$service_id))
      
      gtfs_data$trips          <- subset(gtfs_data$trips,          service_id %chin% service_ids)
      gtfs_data$calendar_dates <- subset(gtfs_data$calendar_dates, service_id %chin% service_ids)
    }

    newsize <- object.size(gtfs_data)
  }

  if(prompt_invalid){
    for(element in names(removed)){
      len <- length(removed[[element]])
      
      if(len > 0)
        message(paste(len, element, "removed:", paste0(removed[[element]], collapse = ", ")))
    }
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
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' 
#' subset <- filter_by_shape_id(poa, "T2-1")
filter_by_shape_id <- function(gtfs_data, shape_ids){
  gtfs_data$shapes <- subset(gtfs_data$shapes, shape_id %in% shape_ids)
  gtfs_data$trips <- subset(gtfs_data$trips, shape_id %in% shape_ids)

  trip_ids <- unique(gtfs_data$trips$trip_id)
  gtfs_data$stop_times <- subset(gtfs_data$stop_times, trip_id %chin% trip_ids)
  
  if(!is.null(gtfs_data$frequencies))
    gtfs_data$frequencies <- subset(gtfs_data$frequencies, trip_id %chin% trip_ids)
  
  stop_ids <- unique(gtfs_data$stop_times$stop_id)
  gtfs_data$stops <- subset(gtfs_data$stops, stop_id %in% stop_ids)
  
  if(!is.null(gtfs_data$routes)){
    route_ids <- unique(gtfs_data$trips$route_id)
    gtfs_data$routes <- subset(gtfs_data$routes, route_id %in% route_ids)  
  }
  
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
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#'
#' result <- filter_by_agency_id(poa, "EPTC")
filter_by_agency_id <- function(gtfs_data, agency_ids){
  if(is.null(gtfs_data$agency)) stop("GTFS data does not have agency")
  if(is.null(gtfs_data$routes)) stop("GTFS data does not have routes")

  gtfs_data$agency <- subset(gtfs_data$agency, agency_id %in% agency_ids)
  gtfs_data$routes <- subset(gtfs_data$routes, agency_id %in% agency_ids)

  route_ids <- unique(gtfs_data$routes$route_id)
  gtfs_data$trips <- subset(gtfs_data$trips, route_id %in% route_ids)
  
  trip_ids <- unique(gtfs_data$trips$trip_id)
  
  if(!is.null(gtfs_data$frequencies))
    gtfs_data$frequencies <- subset(gtfs_data$frequencies, trip_id %chin% trip_ids)
  
  gtfs_data$stop_times <- subset(gtfs_data$stop_times, trip_id %chin% trip_ids)
  
  service_ids <- unique(gtfs_data$trips$service_id)
  
  if(!is.null(gtfs_data$calendar))
    gtfs_data$calendar <- subset(gtfs_data$calendar, service_id %chin% service_ids)
  
  shapes_ids <- unique(gtfs_data$trips$shape_id)
  gtfs_data$shapes <- subset(gtfs_data$shapes, shape_id %in% shapes_ids)
  
  stop_ids <- unique(gtfs_data$stop_times$stop_id)
  gtfs_data$stops <- subset(gtfs_data$stops, stop_id %in% stop_ids)
  
  return(gtfs_data)
}

#' @title Filter GTFS data using valid stop times
#' 
#' @description Filter a GTFS data read using gtfs2gps::read_gtfs(). It removes stop_times
#' with NA values in arrival_time, departure_time, and arrival_time_hms. It also filters
#' stops and routes accordingly.
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' 
#' subset <- filter_valid_stop_times(poa)
filter_valid_stop_times <- function(gtfs_data){
  gtfs_data$stop_times <- subset(gtfs_data$stop_times, !is.na(arrival_time) & !is.na(departure_time))

  stop_ids <- unique(gtfs_data$stop_times$stop_id)
  gtfs_data$stops <- subset(gtfs_data$stops, stop_id %in% stop_ids)
  
  if(!is.null(gtfs_data$routes)){
    route_ids <- unique(gtfs_data$trips$route_id)
    gtfs_data$routes <- subset(gtfs_data$routes, route_id %in% route_ids)
  }

  return(gtfs_data)
}

#' @title Filter GTFS trips operating on given days
#' 
#' @description Filter a GTFS data read using gtfs2gps::read_gtfs(). It removes the
#' trips operating in non-selected days. Note that it might produce
#' inconsistent outputs that can be removed by using gtfs2gps::remove_invalid().
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @param days A vector of selected week days written in the same way of column names
#' in calendar file of GTFS (sunday, monday, etc.).
#' @return A filtered GTFS data with the trips only from the selected days. 
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' 
#' subset <- filter_by_day(poa, c("wednesday", "friday"))
filter_by_day <- function(gtfs_data, days){
  if(!is.null(gtfs_data$calendar_dates)){
    # do not use data.table operators to avoid changing the argument itself
    gtfs_data$calendar_dates$mdate <- as.Date(paste(gtfs_data$calendar_dates$date), format = "%Y%m%d")

    w_days <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday")
    gtfs_data$calendar_dates$mdate <- w_days[as.POSIXlt(gtfs_data$calendar_dates$mdate)$wday + 1]
    
    gtfs_data$calendar_dates <- subset(gtfs_data$calendar_dates, mdate %in% days)
    serviceids <- unique(gtfs_data$calendar_dates$service_id)
    gtfs_data$trips <- subset(gtfs_data$trips, service_id %in% serviceids)
    gtfs_data$calendar_dates$mdate <- NULL
  }
  else if(!is.null(gtfs_data$calendar)){
    code <- parse(text = paste(days, "> 0", collapse = " | "))
    calendar_temp <- subset(gtfs_data$calendar, eval(code))
    serviceids <- unique(calendar_temp$service_id)
    gtfs_data$trips <- subset(gtfs_data$trips, service_id %in% serviceids)
    
    gtfs_data$calendar[, (days) := 0]
  }
  else stop("GTFS data does not have calendar_dates nor calendar")

  return(gtfs_data)
}

#' @title Filter GTFS trips operating on week days
#' 
#' @description Filter a GTFS data read using gtfs2gps::read_gtfs(). It removes the
#' trips operating only on Saturdays or Sundays. Note that it might produce
#' inconsistent outputs that can be removed by using gtfs2gps::remove_invalid().
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' 
#' subset <- filter_week_days(poa)
filter_week_days <- function(gtfs_data){
  if(is.null(gtfs_data$calendar)) stop("GTFS data does not have calendar")
  calendar_temp <- subset(gtfs_data$calendar, monday > 0 | tuesday > 0 | wednesday > 0 | thursday > 0 | friday > 0)
  serviceids <- calendar_temp$service_id
  gtfs_data$trips <- subset(gtfs_data$trips, service_id %in% serviceids)
  gtfs_data$calendar[, sunday := 0]
  gtfs_data$calendar[, saturday := 0]
  
  return(gtfs_data)
}

#' @title Filter GTFS trips in order to have one trip per shape_id
#' 
#' @description Filter a GTFS data by keeping only one trip per shape_id.
#' It also removes the unnecessary routes accordingly.
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' 
#' subset <- filter_single_trip(poa)
filter_single_trip <- function(gtfs_data){
  gtfs_data$trips <- gtfs_data$trips[!duplicated(gtfs_data$trips$shape_id), ]

  if(!is.null(gtfs_data$frequencies)){
    trip_ids <- unique(gtfs_data$trips$trip_id)
    gtfs_data$frequencies <- subset(gtfs_data$frequencies, trip_id %chin% trip_ids)
  }
  
  if(!is.null(gtfs_data$routes)){
    route_ids <- unique(gtfs_data$trips$route_id)
    gtfs_data$routes <- subset(gtfs_data$routes, route_id %in% route_ids)
  }

  return(gtfs_data)
}

#' @title Filter GTFS data by transport mode  (route type)
#' 
#' @description Filter a GTFS data by transport mode (coded in the column route_type 
#' in routes.txt). It also removes the  unnecessary trips, stop_times, shapes, 
#' frequencies (if exist in a feed), and stops accordingly.
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @param route_types A vector of route types belonging to the routes of the
#' gtfs_data data. Note that route_type might be loaded by gtfs2gps::read_gtfs()
#' as a string or a number, depending on the available values.
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' warsaw <- read_gtfs(system.file("extdata/warsaw.zip", package="gtfs2gps"))
#' 
#' subset <- filter_by_route_type(warsaw, c(0, 3))
filter_by_route_type <- function(gtfs_data, route_types) {
  if(is.null(gtfs_data$routes)) stop("GTFS data does not have routes")

  gtfs_data$routes <- subset(gtfs_data$routes, route_type %in% route_types)

  route_ids <- unique(gtfs_data$routes$route_id)
  gtfs_data$trips <- subset(gtfs_data$trips, route_id %in% route_ids) 

  shape_ids <- unique(gtfs_data$trips$shape_id)
  gtfs_data$shapes <- subset(gtfs_data$shapes, shape_id %in% shape_ids)
    
  trip_ids <- unique(gtfs_data$trips$trip_id)
  gtfs_data$stop_times <- subset(gtfs_data$stop_times, trip_id %chin% trip_ids)

  if(!is.null(gtfs_data$frequencies))
    gtfs_data$frequencies <- subset(gtfs_data$frequencies, trip_id %chin% trip_ids)
  
  stop_ids <- unique(gtfs_data$stop_times$stop_id)
  gtfs_data$stops <- subset(gtfs_data$stops, stop_id %in% stop_ids)
  
  return(gtfs_data)
}

#' @title Filter GTFS data by route ids
#' 
#' @description Filter a GTFS data by its route ids, subsetting routes
#' and trips. It also removes the
#' unnecessary stop_times, shapes, frequencies (if exist in a feed), 
#' and stops accordingly.
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @param route_ids A vector of route ids belonging to the routes of the
#' gtfs_data data. Note that route_id might be loaded by gtfs2gps::read_gtfs()
#' as a string or a number, depending on the available values.
#' @return A filtered GTFS data. 
#' @export
#' @examples
#' warsaw <- read_gtfs(system.file("extdata/warsaw.zip", package="gtfs2gps"))
#' 
#' subset <- filter_by_route_id(warsaw, c("15", "175"))
filter_by_route_id <- function(gtfs_data, route_ids) {
  if(is.null(gtfs_data$routes)) stop("GTFS data does not have routes")

  gtfs_data$routes <- subset(gtfs_data$routes, route_id %in% route_ids)
  gtfs_data$trips <- subset(gtfs_data$trips, route_id %in% route_ids) 

  shape_ids <- unique(gtfs_data$trips$shape_id)
  gtfs_data$shapes <- subset(gtfs_data$shapes, shape_id %in% shape_ids)
    
  trip_ids <- unique(gtfs_data$trips$trip_id)
  gtfs_data$stop_times <- subset(gtfs_data$stop_times, trip_id %chin% trip_ids)
  
  if(!is.null(gtfs_data$frequencies))
    gtfs_data$frequencies <- subset(gtfs_data$frequencies, trip_id %chin% trip_ids)
  
  stop_ids <- unique(gtfs_data$stop_times$stop_id)
  gtfs_data$stops <- subset(gtfs_data$stops, stop_id %in% stop_ids)
  
  return(gtfs_data)
}
