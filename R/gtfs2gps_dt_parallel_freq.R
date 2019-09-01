# DIST entre paradas (velocidade variavel)
# freia velocidade perto de paradas

gtfs2gps_dt_parallel_freq <- function(){

setwd("R:/Dropbox/git_projects/gtfs2vein")
# setwd("C:/Users/r1701707/Desktop/gtfs2vein")

library(utils)
library(Rcpp)
library(sf)
library(magrittr)
library(mapview)
library(dplyr)
library(data.table)
library(fasttime)
library(future.apply)
gc(reset = T)


# gtfszip <- "./data/GTFS_POA_20190415.zip"
gtfszip <- "R:/Dropbox/bases_de_dados/GTFS/SP GTFS/GTFS Sptrans_20190815.zip"


gtfs2gps_dt_freq <- function(gtfszip, week_days=T){


###### PART 1. Load and prepare data inputs ------------------------------------

# Use GForce Optimisations in data.table operations
# details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
options(datatable.optimize=Inf)
gc(reset = T)


# Read GTFS data
  source("./R/read_gtfs.R")
  read_gtfs(gtfszip = gtfszip)
    

# Filter trips
  if( week_days==T ){
    # keep only services operating on week days
      calendar_temp <- calendar[ monday >0 | tuesday  >0 | wednesday  >0 | thursday  >0 | friday  >0, ]
      serviceids <- calendar_temp$service_id
      trips <- trips[ service_id %in% serviceids]
      }
  
# Prepare stop times data
# (1 get current date, (2) paste date and (3) convert depart_time to "POSIXt" format
  date_temp <- Sys.Date()
  stoptimes[!is.na(departure_time) & departure_time != "", departure_time :=  paste(date_temp, departure_time)]
  stoptimes[, departure_time := fasttime::fastPOSIXct(departure_time, tz="GMT", required.components = 6) ]
  # ? perhaps use data.table native format >>> data.table::as.ITime("05:20:00") 

  
# Prepare frequencies data
 if(exists("frequencies")){
   frequencies[, start_time :=  paste(date_temp, start_time)][, end_time :=  paste(date_temp, end_time)]
   frequencies[, start_time := fasttime::fastPOSIXct(start_time, tz="GMT", required.components = 6) ]
   frequencies[, end_time := fasttime::fastPOSIXct(end_time, tz="GMT", required.components = 6) ]
   }
  
  
# Prepare shapes data
  # Convert all shapes into sf object
  
  
  shapes_sf <- gtfs_shapes_as_sf(shapes)
  head(shapes_sf)
  
  # all shape ids
  all_shapeids <- unique(shapes_sf$shape_id)
  
  
  # Progress bar start
  total <- length(all_shapeids)
  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)

  
  
  
###### PART 2.1 Core function to work on each Shape id ------------------------------------
corefun <- function(shapeid){ 
  
  # #get a list of all trip ids
  # all_shapeids <- unique(shapes_sf$shape_id)
  # all_shapeids <- all_shapeids[1:100]
  # shapeid <- all_shapeids[1]
  
  
  # Progress bar input
  i <- match(c(shapeid), all_shapeids)
  # Progress bar update
  utils::setTxtProgressBar(pb, i)

  
  
# Check if there is a route associated with that shape
  # Skip shape_id IF there is no route_id associated with that shape_id
  routeid <- trips[shape_id==shapeid]$route_id[1]
  
  if(is.na(routeid)){
   # message(paste0('skipping ', shapeid))
    return(NULL)
    
  }else{
# Select corresponding route, route type, stops and shape of that trip
    routeid <- trips[shape_id==shapeid]$route_id[1]
    routetype <- routes[route_id ==routeid ]$route_type
    
    # trips
    trips_temp <- trips[shape_id== shapeid & route_id== routeid, ]
    all_tripids <- unique(trips_temp$trip_id)
    
    # stops sequence with lat long
    # each shape_id only have one stop sequence
    stops_seq <- stoptimes[trip_id == all_tripids[1], .(stop_id, stop_sequence)] # get stop sequence
    stops_seq[stops, on= "stop_id", c('stop_lat', 'stop_lon') := list(i.stop_lat, i.stop_lon)] # add lat long info
    
    # convert stops to sf
    stops_sf <- gtfs_stops_as_sf(stops_seq)
    
    
    
    # shape
    shape_sf_temp <- subset(shapes_sf, shape_id == shapeid)
    
  # Use point interpolation to get shape with higher spatial resolution
    shp_length <- shape_sf_temp %>% sf::st_sf() %>% sf::st_set_crs(4326) %>% sf::st_length() %>% as.numeric() # in meters
    spatial_resolution = 15 # meters
    sampling <- shp_length / spatial_resolution
    shape_sf_temp <- sf::st_line_sample(shape_sf_temp, n = sampling ) %>% sf::st_cast("LINESTRING")
    
    # get shape points in high resolution
    new_shape <- shape_sf_temp %>% sf::st_cast("POINT") %>% sf::st_sf()
    
    
  # snap stops to route shape
    source("./R/fun_snap_points.R") # snap stops to route shape
    stops_snapped_sf <- st_snap_points(stops_sf, new_shape)

    
  # update stops_seq lat long with snapped coordinates
    stops_seq$stop_lon <- sf::st_coordinates(stops_snapped_sf)[,1]
    stops_seq$stop_lat <- sf::st_coordinates(stops_snapped_sf)[,2]
    
    
    
### Start building new stop_times.txt file
    
    # get shape points in high resolution
    new_stoptimes <- data.table(shape_id = new_shape$shape_id[1],
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
    

    
    
    
###### PART 2.2 Calculate new stop_times for all departures of each each trip id / Shape id ------------------------------------
# Each shape id only has one trip id
# For each shape_id/trip_trip, the  service frequency varies across different period of the day

    
### Create a GPS-like data set of the trip
   tripid <- all_tripids

# if(exists("frequencies") & length(all_tripids)==1)

# add trip_id and route_id
  new_stoptimes[, trip_id := tripid]
  # stoptimes[ trip_id == tripid, ]
  

# Add departure_time
  new_stoptimes[stoptimes, on = c('trip_id', 'stop_id'), 'departure_time' := i.departure_time]

# Get trip duration  between 1st and last stop
  time_at_first_stop <- new_stoptimes[!is.na(departure_time), first(departure_time)]
  time_at_last_stop <- new_stoptimes[!is.na(departure_time), last(departure_time)]
  trip_duration <- difftime(time_at_last_stop, time_at_first_stop, units="hours")
  
# Get trips distance between 1st and last stop ( in KM)
  id_first_stop <- new_stoptimes[departure_time==time_at_first_stop]$id
  id_last_stop <- new_stoptimes[departure_time==time_at_last_stop]$id
  trip_dist <- new_stoptimes[ id %between% list(id_first_stop, id_last_stop), sum(dist)] /1000 # in Km

# Trip average speed (in Km/h)
  trip_speed <- trip_dist / as.numeric(trip_duration)
  
  
# reorder columns
  data.table::setcolorder(new_stoptimes, c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "departure_time", "stop_id", "stop_sequence", "dist"))
  #head(new_stoptimes)
  
# add cummulative distance in KM
  new_stoptimes[, cumdist := cumsum(dist)/1000]
  
# find position of first non-missing departure_time
  pos_non_NA <- new_stoptimes$departure_time
  pos_non_NA <- Position(function(pos_non_NA) !is.na(pos_non_NA), pos_non_NA)
  
  
# distance from trip start to 1st stop
  dist_1st <- new_stoptimes[id== pos_non_NA]$cumdist/1000 # in Km
# get the depart time from 1st stop
  departtime_1st <- new_stoptimes[id== pos_non_NA]$departure_time %>% as.POSIXct()
  departtime_1st <- departtime_1st - (dist_1st/trip_speed*60) # time in seconds
  
  
# Determine the start time of the trip (time stamp of the 1st GPS point of the trip)
  # DELETE class(new_stoptimes$departure_time)
  new_stoptimes[, departure_time := fasttime::fastPOSIXct(departure_time, tz="GMT", required.components = 6) ]
  new_stoptimes[id==1, departure_time := departtime_1st ] 
  
  
# recalculate time stamps for a general example (what we really need is the time elapsed between points)
  new_stoptimes[ departure_time== departure_time[1L], departure_time := departure_time[1L] ]
  new_stoptimes[ , departure_time := first(departure_time) + ( cumdist/trip_speed*3600) ] #get travel time in seconds

  
# Get freq infor for that trip
  freq_temp <- subset(frequencies, trip_id== tripid)
  
# number of trips
  freq_temp[, service_duration := as.numeric(difftime(freq_temp$end_time[1], freq_temp$start_time[1], units="secs")) ]
  freq_temp[, number_of_departures := ceiling(service_duration/headway_secs) ]
  
# add unique rowid
  freq_temp[, period := .I]
  
# get all start times of each period
  all_starttimes <- freq_temp$start_time
  
  


  
  
###### PART 2.2 Function to calculate new stop_times for all departures of each each trip id / Shape id ------------------------------------
update_newstoptimes_freq <- function(starttime){
  
  # starttime <- all_starttimes[1]
  
  # Get headway of each start_time
  thisheadway <- subset(freq_temp, start_time== starttime)$headway_secs
  nmber_of_departures <- subset(freq_temp, start_time== starttime)$number_of_departures
  
  # list of departures
  departure_list <- 1:nmber_of_departures
  
  # # Replicate one new_stop_times for each departure  
  # all_departures <- rep(list(new_stoptimes), nmber_of_departures)
  dt_list <- replicate(4, list(copy(new_stoptimes)))
  
  
  # Function to update stoptimes of each departure
  update_departure_stoptimes <- function(i){ 
    # i <- 4
    
    # Update 1st departure time
    dt_list[[i]][ departure_time==first(departure_time), departure_time := starttime]
    
    # Updating all other stop times according to travel speed and distances
    dt_list[[i]][, departure_time:= departure_time[1L] + ( cumdist/ trip_speed*3660)]
    
    
    # Updating all stop times by adding the headway
    dt_list[[i]][, departure_time:= departure_time + ((i-1)* thisheadway) ]
  }
  
  # Apply function and return the stop times of all departures from that period
  departure_stoptimes <-  lapply(X=seq_along(dt_list), FUN= update_departure_stoptimes) %>% rbindlist()
  
  
  return(departure_stoptimes)
}
  
}
  # apply 2.2 function to all trip ids of a certain shape id
  shape_stoptimes <- lapply(X=all_starttimes, update_newstoptimes_freq)%>% rbindlist()
  return(shape_stoptimes)
  
  # clean memory
  rm(shape_stoptimes)
  gc(reset = T) # verbose=T ?
  
}
  
  
###### PART 3. Apply Core function in parallel to all shape ids------------------------------------

# Parallel processing using future.apply
   future::plan(future::multiprocess)
   output <- future.apply::future_lapply(X = all_shapeids, FUN=corefun, future.packages=c('data.table', 'sf', 'fasttime', 'Rcpp', 'magrittr')) %>% data.table::rbindlist()
  
   ### Single core
   # all_shapeids <- all_shapeids[1:3]
   # output2 <- pbapply::pblapply(X = all_shapeids, FUN=corefun) %>% data.table::rbindlist()
  

  return(output)
  
# closing progress bar
  close(pb)
}






### Run ======
system.time( test <- gtfs2gps_dt_freq(gtfszip) )



# p <- profvis::profvis( test <- gtfs2gps_dt(gtfszip) )
}
