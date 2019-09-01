
# Rafael
devtools::load_all('R:/Dropbox/git_projects/gtfs2gps')
#devtools::load_all('C:/Users/r1701707/Desktop/geobr')




library(utils)
library(Rcpp)
library(sf)
library(magrittr)
library(mapview)
library(dplyr)
library(data.table)
library(future.apply)





gtfs <-'./data/testdata_poa.zip'

gtfs2gps_dt <- function(gtfszip, week_days=T){


###### PART 1. Load and prepare data inputs ------------------------------------

# Use GForce Optimisations in data.table operations
# details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
options(datatable.optimize=Inf)
gc(reset = T)


# Read GTFS data
  message('reading gtfs\n')
  source('./R/read_gtfs.R')
  gtfs_data <- read_gtfs(gtfszip = gtfszip)
    

# Filter trips 
  if( week_days==T ){
    source('./R/filter_gtfs.R')
    gtfs_data <- filter_week_days(gtfs_data) 
  }
  

# Convert all shapes into sf object
  source('./R/gtfs_as_sf.R')
  shapes_sf <- gtfs_shapes_as_sf(gtfs_data)
  head(shapes_sf)
  
  
  # all shape ids
  all_shapeids <- unique(shapes_sf$shape_id)
  
  
  # Progress bar start
  total <- length(all_shapeids)
  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)

###### PART 2.1 Core function to work on each Shape id ------------------------------------
  message('starting to work on Shape IDs\n')
  
  corefun <- function(shapeid){ 
  
  # #get a list of all trip ids
  # all_shapeids <- unique(shapes_sf$shape_id)
  # all_shapeids <- all_shapeids[1:100]
  # shapeid <- all_shapeids[1]
  # shapeid <- "T51-1"

  
  # Progress bar input
  i <- match(c(shapeid), all_shapeids)
  # Progress bar update
  utils::setTxtProgressBar(pb, i)

  
  
# Select corresponding route, route type, stops and shape of that trip
  
  # Skip shape_id IF there is no route_id associated with that shape_id
  routeid <- gtfs_data$trips[shape_id==shapeid]$route_id[1]
  
  if(is.na(routeid)){
   # message(paste0('skipping ', shapeid))
    return(NULL)
    
  }else{
    routeid <- gtfs_data$trips[shape_id==shapeid]$route_id[1]
    routetype <- gtfs_data$routes[route_id ==routeid ]$route_type
    
    # trips
    trips_temp <- gtfs_data$trips[shape_id== shapeid & route_id== routeid, ]
    all_tripids <- unique(trips_temp$trip_id)
    
    # stops sequence with lat long
    # each shape_id only have one stop sequence
    stops_seq <- gtfs_data$stop_times[trip_id == all_tripids[1], .(stop_id, stop_sequence)] # get stop sequence
    stops_seq[gtfs_data$stops, on= "stop_id", c('stop_lat', 'stop_lon') := list(i.stop_lat, i.stop_lon)] # add lat long info
    
    # convert stops to sf
    stops_sf <- sf::st_as_sf(stops_seq, coords = c('stop_lon', 'stop_lat'), agr="identity")
    
    # shape
    shape_sf_temp <- subset(shapes_sf, shape_id == shapeid)
    
    # Use point interpolation to get shape with higher spatial resolution
    shp_length <- shape_sf_temp %>% sf::st_sf() %>% sf::st_set_crs(4326) %>% sf::st_length() %>% as.numeric()
    spatial_resolution = 15 # meters
    sampling <- ceiling(shp_length / spatial_resolution)
      # ERROR? shape_sf_temp <- sf::st_line_sample(shape_sf_temp, n = sampling ) %>% sf::st_cast("LINESTRING")
      shape_sf_temp2 <- sf::st_segmentize(shape_sf_temp, units::set_units(.015, km) ) %>% sf::st_cast("LINESTRING")
    
    
    # get shape points in high resolution
    new_shape <- shape_sf_temp2 %>% sf::st_cast("POINT") %>% sf::st_sf()
    
    
    # snap stops to route shape
    source("./R/fun_snap_points.R") # snap stops to route shape
    st_crs(stops_sf) <- st_crs(new_shape)
    stops_snapped_sf <- st_snap_points(stops_sf, new_shape)

    
    # update stops_seq lat long with snapped coordinates
    stops_seq$stop_lon <- sf::st_coordinates(stops_snapped_sf)[,1]
    stops_seq$stop_lat <- sf::st_coordinates(stops_snapped_sf)[,2]
    
    
    
### Start building new stop_times.txt file
    
    # get shape points in high resolution
    new_stoptimes <- data.table::data.table(shape_id = new_shape$shape_id[1],
                                id = 1:nrow(new_shape),
                                route_type = routetype,
                                shape_pt_lon = sf::st_coordinates(new_shape)[,1],
                                shape_pt_lat = sf::st_coordinates(new_shape)[,2])
    
    
    # Add stops to shape
    new_stoptimes[stops_seq, on=c(shape_pt_lat="stop_lat"),  c('stop_id', 'stop_sequence') := list(i.stop_id, i.stop_sequence) ]
    
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
    Rcpp::sourceCpp("./src/distance_calcs.cpp") # calculate Distance between successive points
    new_stoptimes[, dist := rcpp_distance_haversine(shape_pt_lat, shape_pt_lon, data.table::shift(shape_pt_lat, type="lead"), data.table::shift(shape_pt_lon, type="lead"), tolerance = 10000000000.0)]
    ### using pure R if C++ does not work
      # source("./R/fun_dthaversine.R")
      # new_stoptimes[, dist2 := dt.haversine(shape_pt_lat, shape_pt_lon, data.table::lead(shape_pt_lat), data.table::lead(shape_pt_lon))]
    
    

###### PART 2.2 Function recalculate new stop_times for each trip id of each Shape id ------------------------------------

### Function to generate the GPS-like data set of each trip_id
update_newstoptimes <- function(tripid){
  
  # tripid <- all_tripids[1]

  # stoptimes
  stoptimes_temp <- gtfs_data$stop_times[ trip_id == tripid]
  
  # Get trip duration and length
  trip_duration <- stoptimes_temp[, difftime(departure_time[.N], departure_time[1L], units="hours") ]
  trip_duration <- as.numeric(trip_duration)
  
  # length of the trip ( in KM)
  trip_dist <- shp_length /1000 # in Km
  trip_speed <- trip_dist / trip_duration
  
  
  # Add departure_time
  new_stoptimes[stoptimes_temp, on = 'stop_id', 'departure_time' := i.departure_time]
  
  # add trip_id
  new_stoptimes[, trip_id := tripid]
  head(new_stoptimes)
  
  
  # reorder columns
  data.table::setcolorder(new_stoptimes, c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "departure_time", "stop_id", "stop_sequence", "dist"))
  head(new_stoptimes)
  
  
  # add cummulative distance
  new_stoptimes[, cumdist := cumsum(dist)]
  
  # find position of first non-missing departure_time
  pos_non_NA <- new_stoptimes$departure_time
  pos_non_NA <- Position(function(pos_non_NA) !is.na(pos_non_NA), pos_non_NA)
  
  
  # distance from trip start to 1st stop
  dist_1st <- new_stoptimes[id== pos_non_NA]$cumdist/1000 # in Km
  # get the depart time from 1st stop
  departtime_1st <- new_stoptimes[id== pos_non_NA]$departure_time %>% as.POSIXct()
  departtime_1st <- departtime_1st - (dist_1st/trip_speed*60) # time in seconds
  
  
  # Determine the start time of the trip (time stamp the 1st GPS point of the trip)
  class(new_stoptimes$departure_time)
  new_stoptimes[, departure_time := fasttime::fastPOSIXct(departure_time, tz="GMT", required.components = 6) ]
  new_stoptimes[id==1, departure_time := departtime_1st ] 
  
  
  # recalculate time stamps
  new_stoptimes[, departure_time:= departure_time[1L] + ( cumdist/ trip_speed*60) ]
  
  return(new_stoptimes)
}
    
    
# apply 2.2 function to all trip ids of a certain shape id
  partial_stoptimes <- lapply(X=all_tripids, FUN=update_newstoptimes) %>% data.table::rbindlist()
  return(partial_stoptimes)

  # 2.2 test in parallel
  #future::plan(future::multiprocess) # parallel processing with the future library 
  #output2.2 <- future.apply::future_lapply(X = all_tripids, FUN=update_newstoptimes) %>% data.table::rbindlist()

  }
  
}

  
###### PART 3. Apply Core function in parallel to all shape ids------------------------------------

# Parallel processing using future.apply
   future::plan(future::multiprocess)
   output <- future.apply::future_lapply(X = all_shapeids, FUN=corefun, future.packages=c('data.table', 'sf', 'Rcpp', 'magrittr')) %>% data.table::rbindlist()
  
   ### Single core
   # output <- lapply(X = all_shapeids, FUN=corefun) %>% data.table::rbindlist()
  
  return(output)
  
# closing progress bar
  close(pb)
}






### Run ======
system.time( test <- gtfs2gps_dt(gtfs) )



# p <- profvis::profvis( test <- gtfs2gps_dt(gtfs) )

