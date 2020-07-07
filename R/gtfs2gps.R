#' @title Convert GTFS to GPS-like data given a spatial resolution
#' 
#' @description Convert GTFS data to GPS format by sampling points using a
#' spatial resolution. This function creates additional points in order to
#' guarantee that two points in a same trip will have at most a given
#' distance, indicated as a spatial resolution.
#' 
#' @param gtfs_data A path to a GTFS file to be converted to GPS, or a GTFS data
#' represented as a list of data.tables.
#' @param spatial_resolution The spatial resolution in meters. Default is 50m.
#' @param parallel Decides whether the function should run in parallel. Defaults to FALSE.
#' When TRUE, it will use all cores available minus one.
#' @param strategy Name of evaluation function to use in future parallel processing. Defaults to 
#' "multiprocess", i.e. if multicore evaluation is supported, that will be used, otherwise
#'  multisession evaluation will be used. Fore details, check ?future::plan().
#' @param progress Show a progress bar. Default is TRUE.
#' @param filepath Output file path. As default, the output is returned in R.
#' When this argument is set, each route is saved into a file within filepath,
#' with the name equals to its id. In this case, no output is returned.
#' @param continue Argument that can be used only with filepath. When TRUE, it
#' skips processing the shape identifiers that were already saved into files.
#' It is useful to continue processing a GTFS file that was stopped for some
#' reason. Default value is FALSE.
#' @return A data.table, where each row represents a GPS point. The following columns are
#' returned (units of measurement in parenthesis): dist and cumdist (meters), cumtime (seconds),
#' shape_pt_lon and shape_pt_lat (degrees), speed (km/h), departure_time (hh:mm:ss), .
#' @export
#' @examples
#' library(dplyr)
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#' subset <- filter_by_shape_id(poa, "T2-1") %>%
#'   filter_single_trip()
#' 
#' poa_gps <- gtfs2gps(subset)
gtfs2gps <- function(gtfs_data, spatial_resolution = 50, parallel = FALSE, strategy = 'multiprocess', progress = TRUE, filepath = NULL, continue = FALSE){
###### PART 1. Load and prepare data inputs ------------------------------------
  if(continue & is.null(filepath))
    stop("Cannot use argument 'continue' without passing a 'filepath'.")
  
  # Unzipping and reading GTFS.zip file
  if(class(gtfs_data) == "character"){
    message(paste("Unzipping and reading", basename(gtfs_data)))
    gtfs_data <- read_gtfs(gtfszip = gtfs_data)
  }
  
  gtfs_data$stop_times[, departure_time := as.numeric(departure_time)]
  gtfs_data$stop_times[, arrival_time := as.numeric(arrival_time)]
  
  if(!is.null(gtfs_data$frequencies)){
    gtfs_data$frequencies[, start_time := as.numeric(start_time)]
    gtfs_data$frequencies[, end_time := as.numeric(end_time)]
  }

  # Convert all shapes into sf objects
  message("Converting shapes to sf objects")
  shapes_sf <- gtfs_shapes_as_sf(gtfs_data)

  ###### PART 2. Analysing data type ----------------------------------------------
  corefun <- function(shapeid){ 
    if(continue){
      file <- paste0(filepath, "/", shapeid, ".txt")
      if(file.exists(file)) return(NULL)
    }

    # test
    # all_shapeids <- unique(shapes_sf$shape_id)
    # shapeid <- all_shapeids[2]
    # message(shapeid)
    
    ## Select corresponding route, route type, stops and shape of that trip

    # identify route id
    routeid <- gtfs_data$trips[shape_id == shapeid]$route_id[1]
    
    # get all trips linked to that route
    all_tripids <- gtfs_data$trips[shape_id == shapeid & route_id == routeid, ]$trip_id %>% unique()

    # nstop = number of valid stops in each trip_id
    nstop <- gtfs_data$stop_times[trip_id %in% all_tripids, .N, by ="trip_id"]$N

    # Get the stops sequence with lat long linked to that route
    # each shape_id only has one stop sequence
    
    if(length(nstop) == 0){
      warning(paste0("Shape '", shapeid, "' has zero stops. Ignoring it."),  call. = FALSE) # nocov
      return(NULL) # nocov
    }
    
    # check stop sequence
    stops_seq <- gtfs_data$stop_times[trip_id == all_tripids[which.max(nstop)], .(stop_id, stop_sequence, departure_time)]
    stops_seq[gtfs_data$stops, on = "stop_id", c('stop_lat', 'stop_lon') := list(i.stop_lat, i.stop_lon)] # add lat long info

    # convert stops to sf
    stops_sf <- sfheaders::sf_point(stops_seq, x = "stop_lon", y = "stop_lat", keep = TRUE)
    sf::st_crs(stops_sf) <- sf::st_crs(shapes_sf)
    
    spatial_resolution <- units::set_units(spatial_resolution / 1000, "km")
    
    # new faster verion using sfheaders
    new_shape <- subset(shapes_sf, shape_id == shapeid) %>%
      sf::st_segmentize(spatial_resolution) %>%
      sfheaders::sf_to_df(fill = TRUE) %>%
      sfheaders::sf_point(x = "x", y = "y", keep = TRUE)

    # convert units of spatial resolution to meters
    spatial_resolution <- units::set_units(spatial_resolution, "m")
    
    # snap stops the nodes of the shape route
    snapped <- cpp_snap_points(stops_sf %>% sf::st_coordinates(), 
                                     new_shape %>% sf::st_coordinates(),
                                     spatial_resolution,
                                     all_tripids[which.max(nstop)])
    
    # Skip shape_id IF there are no snnaped stops
    if(is.null(snapped) | length(snapped) == 0 ){
      warning(paste0("Shape '", shapeid, "' has no snapped stops. Ignoring it."),  call. = FALSE)  # nocov
      return(NULL) # nocov
    }
  
    # If there is less than two valid stops, jump this shape_id
    if(min(nstop) < 2){
      warning(paste0("Shape '", shapeid, "' has less than two stops. Ignoring it."),  call. = FALSE)  # nocov
      return(NULL) # nocov
    }

    # Skip shape_id IF there is no route_id associated with that shape_id
    if(is.na(routeid)){
      warning(paste0("Shape '", shapeid, "' has no route_id. Ignoring it."),  call. = FALSE)  # nocov
      return(NULL) # nocov
    }
      
    # update stops_seq with snap stops to route shape
    stops_seq$ref <- snapped
    
    ### Start building new stop_times.txt file

    # get shape points in high resolution
    new_stoptimes <- data.table::data.table(shape_id = new_shape$shape_id[1],
                                            id = 1:nrow(new_shape),
                                            shape_pt_lon = sf::st_coordinates(new_shape)[,1],
                                            shape_pt_lat = sf::st_coordinates(new_shape)[,2])
    
    # identify route type
    if(!is.null(gtfs_data$routes)){
      routetype <- gtfs_data$routes[route_id == routeid]$route_type
      new_stoptimes[stops_seq$ref, "route_type"] <- routetype
    }

    ## Add stops to new_stoptimes  
    new_stoptimes[stops_seq$ref, "stop_id"] <- stops_seq$stop_id
    new_stoptimes[stops_seq$ref, "stop_sequence"] <- stops_seq$stop_sequence
    new_stoptimes[stops_seq$ref, "departure_time"] <- stops_seq$departure_time

    # calculate Distance between successive points
    new_stoptimes[, dist := rcpp_distance_haversine(shape_pt_lat, shape_pt_lon, data.table::shift(shape_pt_lat, type = "lead"), data.table::shift(shape_pt_lon, type = "lead"), tolerance = 1e10)]
    new_stoptimes <- na.omit(new_stoptimes, cols = "dist")

    if(dim(new_stoptimes)[1] < 2){
      warning(paste0("Shape '", shapeid, "' has less than two stops after conversion. Ignoring it."),  call. = FALSE)  # nocov
      return(NULL) # nocov
    }

    if(length(which(!is.na(new_stoptimes$stop_sequence))) < 2){
      warning(paste0("Shape '", shapeid, "' has less than two stop_sequences after conversion. Ignoring it."),  call. = FALSE)  # nocov
      return(NULL) # nocov
    }

    ###### PART 2.2 Function recalculate new stop_times for each trip id of each Shape id ------------------------------
    new_stoptimes <- lapply(X = all_tripids, FUN = update_freq, new_stoptimes, gtfs_data, all_tripids) %>% data.table::rbindlist()

    if(is.null(new_stoptimes$departure_time)){
      warning(paste0("Shape '", shapeid, "' has no departure_time. Ignoring it."),  call. = FALSE)  # nocov
      return(NULL)  # nocov
    }
    
    new_stoptimes[, departure_time := data.table::as.ITime(departure_time)]

    data.table::setcolorder(new_stoptimes, c("id", "shape_id", "trip_id", "trip_number", "route_type", 
      "shape_pt_lon", "shape_pt_lat", "departure_time", "stop_id", "stop_sequence", "dist", "cumdist",
      "cumtime", "speed"))
    
    if(!is.null(filepath)){ # Write object
      data.table::fwrite(x = new_stoptimes,
             file = paste0(filepath, "/", shapeid, ".txt"))
      return(NULL)
    }

    return(new_stoptimes)
  }

  ###### PART 3. Apply Core function in parallel to all shape ids------------------------------------

  # all shape ids
  all_shapeids <- unique(shapes_sf$shape_id)

  if(parallel == FALSE){
    message(paste('Using 1 CPU core'))
    if(progress) pbapply::pboptions(type = "txt")

    message("Processing the data")
    output <- pbapply::pblapply(X = all_shapeids, FUN = corefun) %>% data.table::rbindlist()
    
    if(progress) pbapply::pboptions(type = "none")
  }
  else
  {  
    # number of cores
    cores <- future::availableCores() - 1
    message(paste('Using', cores, 'CPU cores'))
    
    future::plan(strategy, workers = cores)
    
    message("Processing the data")
    output <- furrr::future_map(.x = all_shapeids, .f = corefun, .progress = progress, 
      .options = furrr::future_options(
        packages = c('data.table', 'sf', 'magrittr', 'Rcpp', 'sfheaders', 'units'))) %>% 
      data.table::rbindlist()
  }

  if(is.null(filepath))
    return(output)
  else
    return(NULL)
}

  
