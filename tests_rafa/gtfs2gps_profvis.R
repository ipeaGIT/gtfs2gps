#' @title Convert GTFS to GPS-like data given a spatial resolution
#' 
#' @description Convert GTFS data to GPS format by sampling points using a
#' spatial resolution. This function creates additional points in order to
#' guarantee that two points in a same trip will have at most a given
#' distance, indicated as a spatial resolution.
#' 
#' @param gtfs_data A path to a GTFS file to be converted to GPS, or a GTFS data
#' represented as a list of data.tables.
#' @param spatial_resolution The spatial resolution in meters. Default is 15m.
#' @param filepath Output file path. As default, the output is returned in R.
#' When this argument is set, each route is saved into a file within filepath,
#' with the name equals to its id. In this case, no output is returned.
#' @param cores Number of cores to be used. Defaults to 1.
#' @param progress Show a progress bar. Default is TRUE.
#' @param continue Argument that can be used only with filepath. When TRUE, it
#' skips processing the shape identifiers that were already saved into files.
#' It is useful to continue processing a GTFS file that was stopped for some
#' reason. Default value is FALSE.
#' @export
#' @examples \donttest{
#' library(gtfs2gps)
#'
#' poa <- gtfs2gps(system.file("extdata/poa.zip", package="gtfs2gps"))
#' }

gtfs_data =system.file("extdata/poa.zip", package="gtfs2gps")
spatial_resolution = 50
cores = 1
progress = TRUE
continue = FALSE

gtfs2gps <- function(gtfs_data, filepath = NULL, spatial_resolution = 15, cores = NULL, progress = TRUE, continue = FALSE){
###### PART 1. Load and prepare data inputs ------------------------------------

  if(continue & is.null(filepath))
    stop("Cannot use argument 'continue' without passing a 'filepath'.")

  # Unzipping and reading GTFS.zip file
  if(class(gtfs_data) == "character"){
    message("Unzipping and reading GTFS.zip file")
    gtfs_data <- read_gtfs(gtfszip = gtfs_data)}

  # Convert all shapes into sf objects
  message("converting shapes to sf objects")
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

    
profvis::profvis({ # 6666666666666666666666666666666666666666666666666666

    ## Select corresponding route, route type, stops and shape of that trip

    # identify route id
    routeid <- gtfs_data$trips[shape_id == shapeid]$route_id[1]
    
    # Skip shape_id IF there is no route_id associated with that shape_id
    if(is.na(routeid)) return(NULL) # nocov
    
    # identify route type
    routetype <- gtfs_data$routes[route_id == routeid]$route_type
    
    # get all trips linked to that route
    all_tripids <- gtfs_data$trips[shape_id == shapeid & route_id == routeid, ]$trip_id %>% unique()

    # Get the stops sequence with lat long linked to that route
    # each shape_id only has one stop sequence
    nstop <- gtfs_data$stop_times[trip_id %in% all_tripids, .N, by ="trip_id"]$N
    stops_seq <- gtfs_data$stop_times[trip_id == all_tripids[which.max(nstop)], .(stop_id, stop_sequence)]
    
    stops_seq[gtfs_data$stops, on = "stop_id", c('stop_lat', 'stop_lon') := list(i.stop_lat, i.stop_lon)] # add lat long info

    # convert stops to sf
    stops_sf <- sf::st_as_sf(stops_seq, coords = c('stop_lon', 'stop_lat'), agr = "identity", crs = sf::st_crs(shapes_sf))

    spatial_resolution <- units::set_units(spatial_resolution / 1000, "km")

    new_shape <- subset(shapes_sf, shape_id == shapeid) %>%
      sf::st_segmentize(spatial_resolution) %>%
      sf::st_cast("LINESTRING") %>%
      sf::st_cast("POINT", warn = FALSE) %>%
      sf::st_sf()

    spatial_resolution <- units::set_units(spatial_resolution, "m")
    
    snapped <- cpp_snap_points(stops_sf %>% sf::st_coordinates(), 
                                     new_shape %>% sf::st_coordinates(),
                                     spatial_resolution,
                                     all_tripids[which.max(nstop)])

    if(is.null(snapped) | length(snapped) == 0){
      return(NULL) # nocov
    }
      
    # update stops_seq with snap stops to route shape
    stops_seq$ref <- snapped
      
    ### Start building new stop_times.txt file

    # get shape points in high resolution
    new_stoptimes <- data.table::data.table(shape_id = new_shape$shape_id[1],
                                            id = 1:nrow(new_shape),
                                            route_type = routetype,
                                            shape_pt_lon = sf::st_coordinates(new_shape)[,1],
                                            shape_pt_lat = sf::st_coordinates(new_shape)[,2])
    
    ## Add stops to new_stoptimes  
    new_stoptimes[stops_seq$ref, "stop_id"] <- stops_seq$stop_id
    new_stoptimes[stops_seq$ref, "stop_sequence"] <- stops_seq$stop_sequence

    # calculate Distance between successive points
    # new_stoptimes[, dist := rcpp_distance_haversine(shape_pt_lat, shape_pt_lon, data.table::shift(shape_pt_lat, type = "lead"), data.table::shift(shape_pt_lon, type = "lead"), tolerance = 1e10)]
    new_stoptimes[ , dist := geosphere::distGeo(matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2),
                                                matrix(c(data.table::shift(shape_pt_lon, type="lead"), data.table::shift(shape_pt_lat, type="lead")), ncol = 2))/1000]
    
    new_stoptimes <- na.omit(new_stoptimes, cols = "dist")

    
  }) # 6666666666666666666666666666666666666666666666666666
    
    
    
    ###### PART 2.2 Function recalculate new stop_times for each trip id of each Shape id ------------------------------
    if(test_gtfs_freq(gtfs_data) == "frequency"){
      new_stoptimes <- lapply(X = all_tripids, FUN = update_freq, new_stoptimes, gtfs_data, all_tripids) %>% data.table::rbindlist()
    }else{
      new_stoptimes <- lapply(X = all_tripids, FUN = update_dt, new_stoptimes, gtfs_data, all_tripids) %>% data.table::rbindlist()
    }

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

  # processing the data
  message("Processing the data")

  # number of cores to use
  if(is.null(cores)){ cores <- data.table::getDTthreads() - 1 }
  if(cores == 0){ cores <- 1 } # nocov

  if(cores == 1){
    message(paste('Using', cores, 'CPU core'))
    if(progress) pbapply::pboptions(type = "txt")

    output <- pbapply::pblapply(X = all_shapeids, FUN = corefun) %>% data.table::rbindlist()
    
    if(progress) pbapply::pboptions(type = "none")
  }
  else
  {  
    message(paste('Using', cores, 'CPU cores'))
    output <- pbSapply(cores, progress, X = all_shapeids, FUN = corefun)
  }

  if(is.null(filepath))
    return(output)
  else
    return(NULL)
}
