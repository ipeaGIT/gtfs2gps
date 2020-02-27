#' @title Convert GTFS to GPS given a spatial resolution
#' @description Convert GTFS data to GPS format by sampling points using a
#' spatial resolution. This function creates additional points in order to
#' guarantee that two points in a same trip will have at most a given
#' distance, indicated as a spatial resolution.
#' @param gtfszip A path to a GTFS file to be converted to GPS.
#' @param spatial_resolution The spatial resolution in meters. Default is 15m.
#' @param week_days Use only the week days? Default is TRUE.
#' @export
#gtfs2gps_dt_parallel <- function(gtfszip, spatial_resolution = 15, week_days = TRUE){
###### PART 1. Load and prepare data inputs ------------------------------------
rm(list=ls()) 
gc(reset = TRUE)

# Read GTFS data
gtfszip = "inst/extdata/fortaleza.zip"
source("R/read_gtfs.R")
gtfs_data <- read_gtfs(gtfszip = gtfszip)



# Filter trips
#if(week_days){
#  gtfs_data <- filter_week_days(gtfs_data) 
#}

# Convert all shapes into sf object
library(sf);library(lwgeom)
source("R/gtfs_as_sf.R")
shapes_sf <- gtfs_shapes_as_sf(gtfs_data)

# all shape ids
all_shapeids <- unique(shapes_sf$shape_id)

# Progress bar start
total <- length(all_shapeids)
#  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)

###### PART 2.1 Core function to work on each Shape id ------------------------------------
#corefun <- function(shapeid){
# #get a list of all trip ids
# all_shapeids <- unique(shapes_sf$shape_id)
# all_shapeids <- all_shapeids[1:100]
shapeid <- all_shapeids[1]

# Progress bar input
i <- match(shapeid, all_shapeids)
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
# get stop sequence
stops_seq <- gtfs_data$stop_times[trip_id == all_tripids[1], .(stop_id, stop_sequence)]

# add lat long info
stops_seq[gtfs_data$stops, on = "stop_id", c('stop_lat', 'stop_lon') := list(i.stop_lat, i.stop_lon)]

# convert stops to sf
stops_sf <- sf::st_as_sf(stops_seq, coords = c('stop_lon', 'stop_lat'), agr = "identity", crs = sf::st_crs(shapes_sf))

# shape
shape_sf_temp <- subset(shapes_sf, shape_id == shapeid)

# Use point interpolation to get shape with higher spatial resolution
shp_length <- shape_sf_temp %>% sf::st_sf() %>% sf::st_length() # %>% as.numeric() # meters

#sampling <- ceiling(shp_length / spatial_resolution)
spatial_resolution <- 15#units::set_units(15, "m")
# ERROR? shape_sf_temp <- sf::st_line_sample(shape_sf_temp, n = sampling ) %>% sf::st_cast("LINESTRING")
break()
shape_sf_temp2 <- sf::st_segmentize(shape_sf_temp, spatial_resolution) %>% sf::st_cast("LINESTRING")  %>% sf::st_cast("MULTIPOINT")
#break()
#shape_sf_temp <-  sf::st_transform(shape_sf_temp,crs="+proj=utm +zone=24 ellps=WGS84")
#shape_sf_temp3 <- sf::st_segmentize(shape_sf_temp, spatial_resolution) %>% sf::st_cast("LINESTRING")
break()
# get shape points in high resolution
new_shape <- sf::st_cast(shape_sf_temp2, "POINT", warn = FALSE) %>% sf::st_sf()

# snap stops to route shape
Rcpp::sourceCpp('src/snap_points.cpp')
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
new_stoptimes[stops_seq, on = c(shape_pt_lat = "stop_lat"), c('stop_id', 'stop_sequence') := list(i.stop_id, i.stop_sequence)]

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
Rcpp::sourceCpp('src/distance_calcs.cpp')
new_stoptimes[, dist := rcpp_distance_haversine(shape_pt_lat, shape_pt_lon, data.table::shift(shape_pt_lat, type="lead"), data.table::shift(shape_pt_lon, type="lead"), tolerance = 10000000000.0)]

#a <- head(new_stoptimes)
new_stoptimes <- sf::st_as_sf(x = new_stoptimes, 
                              coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326)
break()

new_stoptimes <- sf::st_as_sf(x = new_stoptimes, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326)
new_stoptimes <- sf::st_transform(new_stoptimes,crs="+proj=utm +zone=24 ellps=WGS84")
st_distance(new_shape$geometry[1],new_shape$geometry[2])
st_distance(new_stoptimes$geometry[2],new_stoptimes$geometry[3])
st_distance(new_stoptimes$geometry[3],new_stoptimes$geometry[4])



new_stoptimes$shape_pt_lat <- sf::st_coordinates(new_stoptimes)[,2]
new_stoptimes$shape_pt_lon <- sf::st_coordinates(new_stoptimes)[,1]
new_stoptimes$geometry <- NULL

new_stoptimes
new_stoptimes[,dist1 := rcpp_distance_haversine(shape_pt_lat, 
                                                shape_pt_lon, 
                                                data.table::shift(shape_pt_lat, type="lead"), 
                                                data.table::shift(shape_pt_lon, type="lead"), 
                                              tolerance = 10000000000.0)]

st_coordinates(new_stoptimes)
dt$geom <- c()
for(i in 1:nrow(dt)){
  dt$geom[i] <- st_sfc(st_point(as.matrix(dt[i,1:2])), crs = 4326)
}
pt <- lapply(1:nrow(dt),function(x){st_point(dt[x,])})

new_stoptimes[,  dist := rcpp_distance_haversine(shape_pt_lat, shape_pt_lon, data.table::shift(shape_pt_lat, type="lead"), 
                                                 data.table::shift(shape_pt_lon, type="lead"), tolerance = 10000000000.0)]
#
# ideas to use average speed between points
# 
# add cummulative distance
new_stoptimes[, cumdist := cumsum(dist)]
stoptimes_temp <- gtfs_data$stop_times[trip_id == tripid]
stop_id_ok <- gtfs_data$stop_times[trip_id == tripid & is.na(departure_time) == FALSE,]$stop_sequence
stop_id_nok <- gtfs_data$stop_times[trip_id == tripid & is.na(departure_time) == TRUE,]$stop_sequence

interp_index_ok <- which(is.na(new_stoptimes$stop_id) == FALSE)
for(i in 1:length(stop_id_ok)){
  dt <- difftime(stoptimes_temp$arrival_time[stop_id_ok[i+1]],
                 stoptimes_temp$departure_time[stop_id_ok[i]])
  ds <- new_stoptimes[stop_sequence==stop_id_ok[i+1],"cumdist"]-
    new_stoptimes[stop_sequence==stop_id_ok[i],"cumdist"]
  mean_speed <- ds/(as.numeric(dt)*60) *3.6
  
  
}
jstop_id <- which(is.na(new_stoptimes$stop_id)==FALSE)
ldt_stopid <- 
  tripid <- all_tripids[1]
jstoptimes_temp <- gtfs_data$stop_times[trip_id == tripid]
gtfs_data$stop_times[]


###### PART 2.2 Function recalculate new stop_times for each trip id of each Shape id ------------------------------------

### Function to generate the GPS-like data set of each trip_id
# update_newstoptimes <- function(tripid){
tripid <- all_tripids[1]

# stoptimes
stoptimes_temp <- gtfs_data$stop_times[trip_id == tripid]

# Get trip duration and length
trip_duration <- stoptimes_temp[, difftime(departure_time[.N], departure_time[1L], units = "hours")]

# length of the trip (in KM)
shp_length <- units::set_units(shp_length, "km")
trip_speed <- as.numeric(shp_length) / as.numeric(trip_duration)

# Add departure_time
new_stoptimes[stoptimes_temp, on = 'stop_id', 'departure_time' := i.departure_time]

# add trip_id
new_stoptimes[, trip_id := tripid]

# reorder columns
data.table::setcolorder(new_stoptimes, c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "departure_time", "stop_id", "stop_sequence", "dist"))

# add cummulative distance
new_stoptimes[, cumdist := cumsum(dist)]
#
break()
# find position of first non-missing departure_time
pos_non_NA <- new_stoptimes$departure_time
pos_non_NA <- Position(function(pos_non_NA) !is.na(pos_non_NA), pos_non_NA)
# joao

#left_join para pegar velocidade


pos_non_NA <-  which(is.na(new_stoptimes$departure_time)==F)[1]
# distance from trip start to 1st stop
dist_1st <- new_stoptimes[id == pos_non_NA]$cumdist / 1000 # in Km
# get the depart time from 1st stop
departtime_1st <- new_stoptimes[id == pos_non_NA]$departure_time
departtime_1st <- departtime_1st - (dist_1st / trip_speed * 60 * 60) # time in seconds

# Determine the start time of the trip (time stamp the 1st GPS point of the trip)
suppressWarnings(new_stoptimes[id == 1, departure_time := data.table::as.ITime(departtime_1st)])

# recalculate time stamps
new_stoptimes[, departure_time := data.table::as.ITime(departure_time[1L] + (cumdist/1000 / trip_speed * 60 * 60))]

# return(new_stoptimes)
#}

# apply 2.2 function to all trip ids of a certain shape id
lapply(X = all_tripids, FUN = update_newstoptimes) %>% data.table::rbindlist()

# 2.2 test in parallel
#output2.2 <- future.apply::future_lapply(X = all_tripids, FUN=update_newstoptimes) %>% data.table::rbindlist()
#}

###### PART 3. Apply Core function in parallel to all shape ids------------------------------------

# Parallel processing using future.apply
future::plan(future::multiprocess)
output <- future.apply::future_lapply(X = all_shapeids, FUN = corefun, future.packages = c('data.table', 'sf', 'Rcpp', 'magrittr')) %>% data.table::rbindlist()
future::plan(future::sequential)
### Single core
# output <- lapply(X = all_shapeids, FUN=corefun) %>% data.table::rbindlist()

# closing progress bar
#  close(pb)
#return(output)
#}