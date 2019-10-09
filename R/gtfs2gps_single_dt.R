#' @title Convert GTFS to GPS given a spatial resolution
#' @description Convert GTFS data to GPS format by sampling points using a
#' spatial resolution. This function creates additional points in order to
#' guarantee that two points in a same trip will have at most a given
#' distance, indicated as a spatial resolution.
#' @param gtfszip A path to a GTFS file to be converted to GPS.
#' @param spatial_resolution The spatial resolution in meters. Default is 15m.
#' @param week_days Use only the week days? Default is TRUE.
#' @export
#'   # my changes #### 
gtfs2gps_dt_single <- function(gtfszip, filepath, spatial_resolution = 15, week_days = TRUE){
  #
  #
  #
  ###### PART 1. Load and prepare data inputs ------------------------------------
  # 
  #
  #
  gc(reset = TRUE)
  
  # Read GTFS data
  gtfs_data <- read_gtfs(gtfszip = gtfszip)
  
  # Filter trips
  if(week_days){
    gtfs_data <- filter_week_days(gtfs_data) 
  }
  
  # Convert all shapes into sf object
  shapes_sf <-  gtfs_shapes_as_sf(gtfs_data)
  
  # all shape ids
  all_shapeids <- unique(shapes_sf$shape_id)
  # all_shapeids <- unique(shapes_sf$shape_id)
  # Progress bar start
  #total <- length(all_shapeids)
  #shapeid <- last(all_shapeids)
  #  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  #
  #
  #
  ###### PART 2. Analysing data type ----------------------------------------------
  # 
  #
  #
  corefun <- function(shapeid){ 
    # #get a list of all trip ids
     all_shapeids <- unique(shapes_sf$shape_id)#; shapeid <- all_shapeids[1]
    #all_shapeids <- all_shapeids[1]
    # break()
    # shapeid <- all_shapeids[1]
    # Progress bar input
    i <- match(shapeid, all_shapeids)
    #print(i)
    # Progress bar update
    #    utils::setTxtProgressBar(pb, i)
    
    # Select corresponding route, route type, stops and shape of that trip
    
    # Skip shape_id IF there is no route_id associated with that shape_id
    routeid <- gtfs_data$trips[shape_id == shapeid]$route_id[1]
    
    if(is.na(routeid)) return(NULL)
    
    routetype <- gtfs_data$routes[route_id == routeid]$route_type
    
    # trips
    trips_temp <- gtfs_data$trips[shape_id == shapeid & route_id == routeid, ]
    all_tripids <- unique(trips_temp$trip_id)
    
    # stops sequence with lat long
    # each shape_id only have one stop sequence
    stops_seq <- gtfs_data$stop_times[trip_id == all_tripids[1], .(stop_id, stop_sequence)]    # get stop sequence
    stops_seq[gtfs_data$stops, on = "stop_id", c('stop_lat', 'stop_lon') := list(i.stop_lat, i.stop_lon)]    # add lat long info
    
    # convert stops to sf
    stops_sf <- sf::st_as_sf(stops_seq, coords = c('stop_lon', 'stop_lat'), agr = "identity", crs = sf::st_crs(shapes_sf))
    
    # shape
    shape_sf_temp <- subset(shapes_sf, shape_id == shapeid)
    
    # Use point interpolation to get shape with higher spatial resolution
    shp_length <- shape_sf_temp %>% sf::st_sf() %>% sf::st_length() # %>% as.numeric() # meters
    #sampling <- ceiling(shp_length / spatial_resolution)
    spatial_resolution <- units::set_units(15 / 1000, "km")
    # ERROR? shape_sf_temp <- sf::st_line_sample(shape_sf_temp, n = sampling ) %>% sf::st_cast("LINESTRING")
    shape_sf_temp2 <- sf::st_segmentize(shape_sf_temp, spatial_resolution) %>% sf::st_cast("LINESTRING")
    # get shape points in high resolution
    new_shape <- sf::st_cast(shape_sf_temp2, "POINT", warn = FALSE) %>% sf::st_sf()
    spatial_resolution <- units::set_units(spatial_resolution, "m")
    # update stops_seq with snap stops to route shape
    stops_seq$ref <- cpp_snap_points(stops_sf %>% sf::st_coordinates(), 
                                     new_shape %>% sf::st_coordinates(), spatial_resolution)
    ### Start building new stop_times.txt file
    
    # get shape points in high resolution
    new_stoptimes <- data.table::data.table(shape_id = new_shape$shape_id[1],
                                            id = 1:nrow(new_shape),
                                            route_type = routetype,
                                            shape_pt_lon = sf::st_coordinates(new_shape)[,1],
                                            shape_pt_lat = sf::st_coordinates(new_shape)[,2])
    
    new_stoptimes[stops_seq$ref, "stop_id"] <- stops_seq$stop_id
    new_stoptimes[stops_seq$ref, "stop_sequence"] <- stops_seq$stop_sequence
    #Add stops to shape ( fixing issue of issue #17 of  routes that are closed circuits represented on a single line)
    # new_stoptimes[stops_seq, on=c(shape_pt_lat="stop_lat"),  c('stop_id', 'stop_sequence') := list(i.stop_id, i.stop_sequence) ]
    # 
    # # make sure 1st stop has postion 1
    # new_stoptimes$stop_sequence[which(!is.na(new_stoptimes$stop_sequence))][1] <- 1 
    max_stoptimes <- dim(new_stoptimes)[1]
    max_stops_seq <- dim(stops_seq)[1]
    j <- 1
    for(i in 1:max_stoptimes){
      if(all.equal(new_stoptimes$shape_pt_lon[i], stops_seq$stop_lon[j], 0.0000001) == TRUE &&
         all.equal(new_stoptimes$shape_pt_lat[i], stops_seq$stop_lat[j], 0.0000001) == TRUE){
        new_stoptimes[i, "stop_id"] <- stops_seq[j, "stop_id"]
        new_stoptimes[i, "stop_sequence"] <- stops_seq[j, "stop_sequence"]
        j <- j + 1
      }
    }
    # calculate Distance between successive points
    # using C++ : Source: https://stackoverflow.com/questions/36817423/how-to-efficiently-calculate-distance-between-pair-of-coordinates-using-data-tab?noredirect=1&lq=1
    new_stoptimes[, dist := gtfs2gps:::rcpp_distance_haversine(shape_pt_lat, shape_pt_lon, data.table::shift(shape_pt_lat, type="lead"), data.table::shift(shape_pt_lon, type="lead"), tolerance = 10000000000.0)]
    new_stoptimes <- na.omit(new_stoptimes, cols = "dist")
    
    
    ###### PART 2.2 Function recalculate new stop_times for each trip id of each Shape id ------------------------------------
    if(test_gtfs_freq(gtfs_data)=="frequency"){
      new_stoptimes <- lapply(X=all_tripids,FUN=update_freq,new_stoptimes) %>% data.table::rbindlist()
    }else{
      new_stoptimes <- lapply(X=all_tripids,FUN=update_dt,new_stoptimes) %>% data.table::rbindlist()
    }
    
    #
    # Write object
    fwrite(x = new_stoptimes,
           file = paste0(filepath,"/",shapeid,".txt"))
    
    return(new_stoptimes)
    
  }
  ###### PART 3. Apply Core function in parallel to all shape ids------------------------------------
  
  # Parallel processing using future.apply
  
  future::plan(future::multiprocess)
  output <- future.apply::future_lapply(X = all_shapeids, 
                                        FUN = corefun, 
                                        future.packages = c('data.table', 'sf', 'Rcpp', 'magrittr')) %>% data.table::rbindlist()
  #### output <- lapply(X = all_shapeids, FUN = corefun) %>% data.table::rbindlist()
  
  ### Single core
  # all_shapeids <- all_shapeids[1:3]
  # output2 <- pbapply::pblapply(X = all_shapeids, FUN=corefun) %>% data.table::rbindlist()
  return(output)
    # # closing progress bar
  #   close(pb)
}