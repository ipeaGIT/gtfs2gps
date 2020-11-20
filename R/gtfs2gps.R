#' @title Convert GTFS to GPS-like data given a spatial resolution
#' 
#' @description Convert GTFS data to GPS format by sampling points using a
#' spatial resolution. This function creates additional points in order to
#' guarantee that two points in a same trip will have at most a given
#' distance, indicated as a spatial resolution. It is possible to use future package
#' to parallelize the execution (or use argument plan). This function also
#' uses progressr internally to show progress bars.
#' 
#' @param gtfs_data A path to a GTFS file to be converted to GPS, or a GTFS data
#' represented as a list of data.tables.
#' @param spatial_resolution The spatial resolution in meters. Default is 50m.
#' @param parallel This argument is deprecated. Please use argument plan instead or
#' use future::plan() directly.
#' @param plan Decides whether the function should run in parallel. Defaults is FALSE.
#' When TRUE, it will use all cores available minus one using future::plan() with
#' strategy "multiprocess" internally.
#' Note that it is possible to create your own plan before calling gtfs2gps().
#' In this case, do not use this argument.
#' @param strategy This argument is deprecated. Please use argument plan instead or
#' use future::plan() directly.
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
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' subset <- filter_by_shape_id(poa, "T2-1") %>%
#'   filter_single_trip()
#' 
#' poa_gps <- gtfs2gps(subset)
gtfs2gps <- function(gtfs_data,
                     spatial_resolution = 50,
                     parallel = NULL,
                     strategy = NULL,
                     filepath = NULL,
                     continue = FALSE,
                     plan = FALSE){

  if(!is.null(parallel))
    warning("Argument 'parallel' is deprecated and will be removed in a future version. Please use 'plan' instead.") # nocov
  
  if(!is.null(parallel))
    warning("Argument 'strategy' is deprecated and will be removed in a future version. Please use 'plan' instead.") # nocov

  ###### PART 1. Load and prepare data inputs ------------------------------------
  if(continue & is.null(filepath))
    stop("Cannot use argument 'continue' without passing a 'filepath'.")
  
  original_gtfs_data_arg <- deparse(substitute(gtfs_data))
  
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
    nstop <- gtfs_data$stop_times[trip_id %chin% all_tripids, .N, by = "trip_id"]$N

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
    
    # new faster version using sfheaders
    new_shape <- subset(shapes_sf, shape_id == shapeid) %>%
      sf::st_segmentize(spatial_resolution) %>%
      sfheaders::sf_cast( "POINT")
    
    # convert units of spatial resolution to meters
    spatial_resolution <- units::set_units(spatial_resolution, "m")
    
    # snap stops the nodes of the shape route
    snapped <- cpp_snap_points(stops_sf %>% sf::st_coordinates(), 
                                     new_shape %>% sf::st_coordinates(),
                                     spatial_resolution,
                                     all_tripids[which.max(nstop)])
    
    # Skip shape_id IF there are no snapped stops
    if(is.null(snapped) | length(snapped) == 0 ){
      warning(paste0("Shape '", shapeid, "' has no snapped stops. Ignoring it."),  call. = FALSE)  # nocov
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
                                            id = seq_len(nrow(new_shape)),
                                            shape_pt_lon = sf::st_coordinates(new_shape)[,1],
                                            shape_pt_lat = sf::st_coordinates(new_shape)[,2])
    
    # identify route type
    if(!is.null(gtfs_data$routes)){
      routetype <- gtfs_data$routes[route_id == routeid]$route_type
      new_stoptimes[, "route_type"] <- routetype
    }

    ## Add stops to new_stoptimes  
    new_stoptimes[stops_seq$ref, "stop_id"] <- stops_seq$stop_id
    new_stoptimes[stops_seq$ref, "stop_sequence"] <- stops_seq$stop_sequence
    new_stoptimes[stops_seq$ref, "departure_time"] <- stops_seq$departure_time

    # calculate Distance between successive points
    new_stoptimes[, dist := rcpp_distance_haversine(shape_pt_lat, shape_pt_lon, data.table::shift(shape_pt_lat, type = "lead"), data.table::shift(shape_pt_lon, type = "lead"), tolerance = 1e10)]
    # new_stoptimes[, dist := rcpp_distance_haversine(shape_pt_lat, shape_pt_lon, data.table::shift(shape_pt_lat, type = "lag"), data.table::shift(shape_pt_lon, type = "lag"), tolerance = 1e10)]
    # new_stoptimes[1, dist := 0]
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

  badShapes <- c()

  # all shape ids
  all_shapeids <- unique(shapes_sf$shape_id)

  if(plan == TRUE)
  {
    # number of cores
    cores <- max(1, future::availableCores() - 1)
    message(paste('Using', cores, 'CPU cores'))
    
    oplan <- future::plan("multiprocess", workers = cores)
    on.exit(future::plan(oplan), add = TRUE)
  }

  p <- progressr::progressor(steps = length(all_shapeids))
  
  tryCorefun <- function(shapeid){
    p()
    result <- NULL
    tryCatch({result <- corefun(shapeid)}, error = function(msg) {
      badShapes <<- c(badShapes, shapeid) # nocov
    })
    
    return(result)
  }
  
  message("Processing the data")
  requiredPackages = c('data.table', 'sf', 'magrittr', 'Rcpp', 'sfheaders', 'units')
  output <- furrr::future_map(.x = all_shapeids, .f = tryCorefun, 
                              .options = furrr::future_options(
                              packages = requiredPackages)) %>% 
    data.table::rbindlist()
  
  if(length(badShapes) > 0){
    if(original_gtfs_data_arg == ".") original_gtfs_data_arg <- "<your gtfs data>" # nocov
    
    message(paste0("Some internal bug occurred while processing gtfs data.\n", # nocov
                   "Please give us a feedback by creating a GitHub issue\n", # nocov
                   "(https://github.com/ipeaGIT/gtfs2gps/issues/new)\n"), # nocov
                   "and attaching a subset of your data created from the\n", # nocov
                   "code below:\n", # nocov
                   "################################################") # nocov
    
    ids <- paste0("ids <- c('", paste(badShapes, collapse = "', '"), "')") # nocov
    code1 <- paste0("data <- gtfs2gps::filter_by_shape_id(", original_gtfs_data_arg, ", ids)") # nocov
    code2 <- "gtfs2gps::write_gtfs(data, 'shapes_with_error.zip')" # nocov
    
    message(paste(ids, code1, code2, sep = "\n")) # nocov
    message("################################################") # nocov
  }

  total_shapes <- length(unique(gtfs_data$shapes$shape_id))
  processed_shapes <- length(unique(output$shape_id))

  if(processed_shapes < total_shapes && is.null(filepath)){
    perc <- round(processed_shapes / total_shapes * 100, 2)
    message(paste0(processed_shapes, " out of ", total_shapes, " shapes (", perc, "%) were properly processed."))
  }

  total_trips <- length(unique(gtfs_data$trips$trip_id))
  processed_trips <- length(unique(output$trip_id))
  
  if(processed_trips < total_trips && is.null(filepath)){
    perc <- round(processed_trips / total_trips * 100, 2)
    message(paste0(processed_trips, " out of ", total_trips, " trips (", perc, "%) were properly processed."))
  }

  if(is.null(filepath)){
    if(any(is.na(output$speed)))
      message("Some 'speed' values are NA in the returned data")
    
    if(any(is.infinite(output$speed)))
      message("Some 'speed' values are Inf in the returned data")
    
    output$speed <- units::set_units(output$speed, "km/h")
    output$dist <- units::set_units(output$dist, "m")
    output$cumdist <- units::set_units(output$cumdist, "m")
    output$cumtime <- units::set_units(output$cumtime, "s")
    
    return(output)
  }
  else
    return(NULL)
}

  
