


gtfs2gps_dt_parallel_igraph <- function(gtfszip, spatial_resolution = 15, week_days = TRUE){
  ###### PART 1. Load and prepare data inputs ------------------------------------
  gc(reset = TRUE)
  
  # gtfszip <- './inst/extdata/saopaulo.zip'
  
  
  # Read GTFS data
  gtfs_data <- read_gtfs(gtfszip = gtfszip)
  
  # Filter trips 
  if(week_days == TRUE){
    gtfs_data <- filter_week_days(gtfs_data) 
  }
  
  # Convert all shapes into sf object
  shapes_sf <- gtfs_shapes_as_sf(gtfs_data)
  
  # all shape ids
  all_shapeids <- unique(shapes_sf$shape_id)
  
  # Progress bar start
  total <- length(all_shapeids)
  #  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  
###### PART 2.1 Core function to work on each Shape id ------------------------------------
corefun <- function(shapeid){
  # #get a list of all trip ids
  # all_shapeids <- unique(shapes_sf$shape_id)
  # all_shapeids <- all_shapeids[1:100]
  # shapeid <- all_shapeids[1]
  
  # Progress bar input
  i <- match(c(shapeid), all_shapeids)
  # Progress bar update
  #    utils::setTxtProgressBar(pb, i)
  
  # Select corresponding route, route type, stops and shape of that trip
  
  # Skip shape_id IF there is no route_id associated with that shape_id
  routeid <- gtfs_data$trips[shape_id == shapeid]$route_id[1]
  
  if(is.na(routeid)){
    return(NULL)
  }else{
    routeid <- gtfs_data$trips[shape_id == shapeid]$route_id[1]
    routetype <- gtfs_data$routes[route_id == routeid]$route_type
    
    # trips
    trips_temp <- gtfs_data$trips[shape_id == shapeid & route_id == routeid, ]
    all_tripids <- unique(trips_temp$trip_id)
    
    # stops sequence with lat long
    # each shape_id only have one stop sequence
    stops_seq <- gtfs_data$stop_times[trip_id == all_tripids[1], .(stop_id, stop_sequence)] # get stop sequence
    stops_seq[gtfs_data$stops, on = "stop_id", c('stop_lat', 'stop_lon') := list(i.stop_lat, i.stop_lon)] # add lat long info
    
    # convert stops to sf
    stops_sf <- sf::st_as_sf(stops_seq, coords = c('stop_lon', 'stop_lat'), agr = "identity")
    
    # shape
    shape_sf_temp <- subset(shapes_sf, shape_id == shapeid)
    
    # Use point interpolation to get shape with higher spatial resolution
    shp_length <- shape_sf_temp %>% sf::st_sf() %>% sf::st_set_crs(4326) %>% sf::st_length() %>% as.numeric()
    
    #sampling <- ceiling(shp_length / spatial_resolution)
    spatial_resolution <- 15/1000
    # ERROR? shape_sf_temp <- sf::st_line_sample(shape_sf_temp, n = sampling ) %>% sf::st_cast("LINESTRING")
    shape_sf_temp2 <- sf::st_segmentize(shape_sf_temp, units::set_units(spatial_resolution, "km") ) %>% sf::st_cast("LINESTRING")
    
    # get shape points in high resolution
    new_shape <- sf::st_cast(shape_sf_temp2, "POINT", warn = FALSE) %>% sf::st_sf()
    
    # snap stops to route shape
    sf::st_crs(stops_sf) <- sf::st_crs(new_shape)
    
    stops_snapped_sf <- cpp_snap_points(stops_sf %>% sf::st_coordinates(), new_shape %>% sf::st_coordinates())
    
    # update stops_seq lat long with snapped coordinates
    stops_seq$stop_lon <- stops_snapped_sf$x
    stops_seq$stop_lat <- stops_snapped_sf$y
    
    ### Start building new stop_times.txt file
    
    # get shape points in high resolution
    new_stoptimes <- data.table::data.table(shape_id = new_shape$shape_id[1],
                                            id = 1:nrow(new_shape),
                                            route_type = routetype,
                                            shape_pt_lon = sf::st_coordinates(new_shape)[,1],
                                            shape_pt_lat = sf::st_coordinates(new_shape)[,2])
    
    # Add stops to shape
    new_stoptimes[stops_seq, on = c(shape_pt_lat="stop_lat"), c('stop_id', 'stop_sequence') := list(i.stop_id, i.stop_sequence) ]
    
    ###check if everything is Ok
    ##kept path
    # a <- new_stoptimes[, .(shape_pt_lon, shape_pt_lat)] %>% as.matrix %>% sf::st_linestring()
    # plot(a)
    ## stop sequence is Ok
    # a <- na.omit(new_stoptimes)
    # plot(a$stop_sequence)
    # plot(stops_seq$stop_sequence)
    # a$stop_sequence == stops_seq$stop_sequence
    
    # calculate Distance between successive points
    # using C++ : Source: https://stackoverflow.com/questions/36817423/how-to-efficiently-calculate-distance-between-pair-of-coordinates-using-data-tab?noredirect=1&lq=1
      # new_stoptimes[, dist := rcpp_distance_haversine(shape_pt_lat, shape_pt_lon, data.table::shift(shape_pt_lat, type="lead"), data.table::shift(shape_pt_lon, type="lead"), tolerance = 10000000000.0)]
          
    new_stoptimes[ , dist := geosphere::distGeo(matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2), 
                                    matrix(c(data.table::shift(shape_pt_lon, type="lead"), data.table::shift(shape_pt_lat, type="lead")), ncol = 2))/1000]
    
    
    
########### IGRAPH ---------------------
    tic()
    
    GraphResult <- data.frame(Source = c(NULL), 
                              Target = c(NULL), 
                              weight  = c(NULL))
    
    for (i in 1:(dim(new_stoptimes)[1] - 1)) {
      
      TempGraphResult <- data.frame(Source = c(0), 
                                    Target = c(0), 
                                    weight  = c(0))
      
      TempGraphResult$Source[1] <- new_stoptimes$id[i]
      TempGraphResult$Target[1] <- new_stoptimes$id[i + 1]
      TempGraphResult$weight[1] <- new_stoptimes$dist[i]
      
      GraphResult <- rbind(GraphResult, TempGraphResult) }
    
    # Creat routable igraph object
    MyIgraph <- graph_from_data_frame(GraphResult) 
    
    
    # # calulate distances
    # distances(MyIgraph, "1", "2") #returns 3.254183. Seems correct (0.1914225*17)
    # SquareMatrix <- distances(MyIgraph)
    
    
    
    toc()
########### 
########### 
    
    
    
    
###### PART 2.2 Function recalculate new stop_times for each trip id of each Shape id ------------------------------------
    
    ### Function to generate the GPS-like data set of each trip_id
    update_newstoptimes <- function(tripid){
      # tripid <- all_tripids[1]
      
      # stoptimes
      stoptimes_temp <- gtfs_data$stop_times[trip_id == tripid]
      
      # Get trip duration and length
      trip_duration <- stoptimes_temp[, difftime(departure_time[.N], departure_time[1L], units = "hours") ]
      trip_duration <- as.numeric(trip_duration)
      
      # length of the trip (in KM)
      trip_dist <- shp_length / 1000 # in Km
      trip_speed <- trip_dist / trip_duration
      
      # Add departure_time
      new_stoptimes[stoptimes_temp, on = 'stop_id', 'departure_time' := i.departure_time]
      
      # add trip_id
      new_stoptimes[, trip_id := tripid]
      
      # reorder columns
      data.table::setcolorder(new_stoptimes, c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "departure_time", "stop_id", "stop_sequence", "dist"))
      
      # add cummulative distance
      new_stoptimes[, cumdist := cumsum(dist)]
      
      # find position of first non-missing departure_time
      pos_non_NA <- new_stoptimes$departure_time
      pos_non_NA <- Position(function(pos_non_NA) !is.na(pos_non_NA), pos_non_NA)
      
      # distance from trip start to 1st stop
      dist_1st <- new_stoptimes[id == pos_non_NA]$cumdist / 1000 # in Km
      # get the depart time from 1st stop
      departtime_1st <- new_stoptimes[id == pos_non_NA]$departure_time
      departtime_1st <- departtime_1st - (dist_1st / trip_speed * 60) # time in seconds
      
      # Determine the start time of the trip (time stamp the 1st GPS point of the trip)
      class(new_stoptimes$departure_time)
      suppressWarnings(new_stoptimes[id == 1, departure_time := data.table::as.ITime(departtime_1st)])
      
      # recalculate time stamps
      new_stoptimes[, departure_time := data.table::as.ITime(departure_time[1L] + (cumdist / trip_speed * 60))]
      
      return(new_stoptimes)
    }
    
    # apply 2.2 function to all trip ids of a certain shape id
    partial_stoptimes <- lapply(X = all_tripids, FUN = update_newstoptimes) %>% data.table::rbindlist()
    return(partial_stoptimes)
    
    # 2.2 test in parallel
    #output2.2 <- future.apply::future_lapply(X = all_tripids, FUN=update_newstoptimes) %>% data.table::rbindlist()
  }
}

###### PART 3. Apply Core function in parallel to all shape ids------------------------------------

# Parallel processing using future.apply
future::plan(future::multiprocess)
output <- future.apply::future_lapply(X = all_shapeids, FUN = corefun, future.packages = c('data.table', 'sf', 'Rcpp', 'magrittr')) %>% data.table::rbindlist()

### Single core
# output <- lapply(X = all_shapeids, FUN=corefun) %>% data.table::rbindlist()

# closing progress bar
#  close(pb)
return(output)
}
