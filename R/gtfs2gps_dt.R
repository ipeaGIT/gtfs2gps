
gtfs2gps_dt <- function(){

setwd("R:/Dropbox/git_projects/gtfs2vein")

setwd("C:/Users/r1701707/Desktop/gtfs2vein")


library(Rcpp)
library(sf)
library(magrittr)
library(mapview)
library(dplyr)
library(data.table)



### Read GTFS data

# data input
gtfs <- "./data/GTFS_POA_20190415.zip"

source("./R/read_gtfs.R")
read_gtfs(gtfszip = gtfs)




### FUNCTION

gtfs2gps_dt <- function(tripid, week_days=T){ 
  
  # tripid <- trips$trip_id[1]

# Use GForce Optimisations in data.table operations
  # details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
  options(datatable.optimize=Inf)
  gc(reset = T)
  

  
            

# Select corresponding route, routetype, stops and shape of that trip
  # route id and type
  routeid <- trips[trip_id==tripid]$route_id
  routetype <- routes[route_id ==routeid ]$route_type
  
  # shape
  shape_temp <- trips[trip_id==tripid ,]$shape_id %>% unique()
  shape_temp <- shapes[shape_id == shape_temp, .(shape_id , shape_pt_lat, shape_pt_lon)]
  
  # stops
  stops_temp <- stoptimes[trip_id == tripid, .(stop_id, stop_sequence)]
  latlong <- stops[ stop_id %in% stops_temp$stop_id, .(stop_id, stop_lat, stop_lon)]
  stops_temp <- stops_temp[latlong, on="stop_id", c('stop_lat', 'stop_lon') := list(i.stop_lat, i.stop_lon) ]
  
# convert stops to sf
  stops_sf <- sf::st_as_sf(stops_temp, coords = c('stop_lon', 'stop_lat'), agr="identity")
  
# convert shape to sf LINESTRING with point interpolation for higher resolution
  shape_sf <- shape_temp[, .(shape_pt_lon, shape_pt_lat)] %>% as.matrix %>% sf::st_linestring()
  shape_sf <- sf::st_line_sample(shape_sf, n = nrow(shape_temp) ) %>% sf::st_cast("LINESTRING")
  
  
  # Use point interpolation to get shape with higher spatial resolution
  shp_length <- shape_sf %>% sf::st_sf() %>% sf::st_set_crs(4326) %>% sf::st_length() %>% as.numeric() 
  spatial_resolution = 15 # meters
  sampling <- ceiling(shp_length / spatial_resolution)
  shape_sf <- sf::st_line_sample(shape_sf, n = sampling ) %>% sf::st_cast("LINESTRING")

  # get shape points in high resolution
  new_shape <- shape_sf %>% sf::st_cast("POINT") %>% sf::st_sf()
  
  
# snap stops to route shape
source("./R/fun_snap_points.R")
stops_snapped_sf <- st_snap_points(stops_sf, new_shape)
# mapview::mapview(shape_temp_sf) + stops_temp_sf + stops_snapped_sf


# update stops_sf lat long with snapped coordinates
  stops_temp$stop_lon <- sf::st_coordinates(stops_snapped_sf)[,1]
  stops_temp$stop_lat <- sf::st_coordinates(stops_snapped_sf)[,2]
  #DELETE> stops_sf <- sf::st_as_sf(stops_temp, coords = c('stop_lon', 'stop_lat'), agr="identity")


  
# stoptimes
  stoptimes_temp <- stoptimes[ trip_id == tripid]
  
  
# Get trip duration and length
stoptimes_temp[, departure_time := as.POSIXct(departure_time, format="%H:%M:%OS")]
trip_duration <- stoptimes_temp[, difftime(departure_time[.N], departure_time[1L], units="hours") ]
trip_duration <- as.numeric(trip_duration)

# length of the trip ( in KM)
trip_dist <- shape_sf %>% sf::st_set_crs(4326) %>% sf::st_length() %>% as.numeric() /1000
trip_speed <- trip_dist / trip_duration




### Start building new stop_times.txt file

# get shape points in high resolution
new_stoptimes <- data.table(shape_id = new_shape$shape_id[1],
                             id = 1:nrow(new_shape),
                             route_type = routetype,
                             shape_pt_lon = sf::st_coordinates(new_shape)[,1],
                             shape_pt_lat = sf::st_coordinates(new_shape)[,2])


# Add stops to shape
new_stoptimes[stops_temp, on=c(shape_pt_lat="stop_lat"),  c('stop_id', 'stop_sequence') := list(i.stop_id, i.stop_sequence) ]



# check if everything is Ok
# kept path
# a <- new_stoptimes[, .(shape_pt_lon, shape_pt_lat)] %>% as.matrix %>% sf::st_linestring()
# plot(a)

# stop sequence is Ok
# a <- na.omit(new_stoptimes)
# plot(a$stop_sequence)
# plot(stops_temp$stop_sequence)
# 
# a$stop_sequence == stops_temp$stop_sequence


#################### commmon per route/direction



# Add arrival_time and departure_time
new_stoptimes[stoptimes_temp, on = 'stop_id',
              c('arrival_time', 'departure_time') := list(i.arrival_time, i.departure_time)]

# add trip_id
new_stoptimes[, trip_id := tripid]
head(new_stoptimes)




# calculate Distance between successive points
# using C++ : Source: https://stackoverflow.com/questions/36817423/how-to-efficiently-calculate-distance-between-pair-of-coordinates-using-data-tab?noredirect=1&lq=1
Rcpp::sourceCpp("./src/distance_calcs.cpp")
new_stoptimes[, dist := rcpp_distance_haversine(shape_pt_lat, shape_pt_lon, data.table::shift(shape_pt_lat, type="lead"), data.table::shift(shape_pt_lon, type="lead"), tolerance = 10000000000.0)]

    # ## using pure R
    # source("./R/fun_dthaversine.R")
    # new_stoptimes[, dist := dt.haversine(shape_pt_lat, shape_pt_lon, data.table::shift(shape_pt_lat, type="lead"), data.table::shift(shape_pt_lon, type="lead"))]


# reorder columns
data.table::setcolorder(new_stoptimes, c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "arrival_time", "departure_time", "stop_id", "stop_sequence", "dist"))
head(new_stoptimes)


# add cummulative distance
new_stoptimes[, cumdist := cumsum(dist)]



# find position of first non-missing value
pos_non_NA <- new_stoptimes$departure_time
pos_non_NA <- Position(function(pos_non_NA) !is.na(pos_non_NA), pos_non_NA)


# distance from trip start to 1st stop
dist_1st <- new_stoptimes[id== pos_non_NA]$cumdist/1000 # in Km
# depart time from 1st stop
departtime_1st <- new_stoptimes[id== pos_non_NA]$departure_time


# Determine the start time of the trip
new_stoptimes[id==1, departure_time := departtime_1st - (dist_1st/trip_speed*60) ] # time in seconds


# recalculate time stamps
new_stoptimes[, departure_time:= departure_time[1L] + ( cumdist/ trip_speed*60) ]

return(new_stoptimes)
}


# Select a tip id
all_tripids <- unique(trips$trip_id)
all_tripids <- all_tripids[1:10]

# # single core
# library(pbapply)
# 
# 
# p <- profvis::profvis( x <- pblapply(X= all_tripids, FUN=gtfs2gps) %>% rbindlist() )


system.time( x <- pbapply::pblapply(X= all_tripids, FUN=gtfs2gps_dt) )
# 10 first trips took 21 seconds
# benchmark


# create computing clusters
cl <- parallel::makeCluster(parallel::detectCores())

parallel::clusterEvalQ(cl, c(library(data.table), library(sf), library(Rcpp), library(magrittr)))
parallel::clusterExport(cl=cl, varlist= c("all_tripids", "routes", "shapes", "stops", "stoptimes", "trips"), envir=environment())




# apply function in parallel
system.time( x <- parallel::parLapply(cl, all_tripids, gtfs2gps_dt) )
x <- rbindlist(x)

# stopCluster(cl)


# Add progress bar
# perhaps using https://github.com/kvnkuang/pbmcapply


}
