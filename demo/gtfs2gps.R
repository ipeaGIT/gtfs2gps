
gtfs2gps <- function(){

library(dplyr)
library(purrr)
library(sf)
library(units)
library(ggplot2)
library(vein)
library(tmap)

setwd("C:/Users/pedro/github/gtfs2vein")

# 1 - LOADING DATA

local_gtfs_path <- "GTFS_POA_20190415.zip"

gtfs <- tidytransit::read_gtfs(local_gtfs_path, 
                               local=TRUE,
                               geometry=TRUE,
                               frequency=TRUE)

splitted <- gtfs$shapes %>%
    split(.$shape_id)

data <- splitted %>%
    map(~ select(., shape_pt_lon, shape_pt_lat) %>%
            as.matrix %>%
            st_linestring) %>%
    st_sfc(crs = 4326) %>% # latlong WGS84
    st_transform(crs = 2263) # TODO: find better projection for POA

mdata <- data.frame(
    geom = data,
    id = names(splitted),
    length = data %>% st_length() %>% set_units(km),
    stringsAsFactors = FALSE) %>%
    st_sf

get_trip_times <- function(gtfs, mtrip_id){
    stop_times <- gtfs$stop_times %>% filter(startsWith(trip_id, mtrip_id)) %>% na.omit()
    
    if(dim(stop_times)[1] == 0) return(NULL) # lines with no trips
    
    finish <- 1:(dim(stop_times)[1]/2) * 2
    start <- finish - 1
    
    trip_times <- stop_times$arrival_time_hms[finish] - stop_times$arrival_time_hms[start]
    
    tibble::tibble(
        trip_id = stop_times$trip_id %>% unique(),
        start = stop_times$arrival_time_hms[start],
        stop_id = stop_times$stop_id[1],
        stop_sequence = stop_times$stop_sequence[1],
        finish = stop_times$departure_time_hms[finish],
        trip_time = trip_times
    )
}

trips = list()

for(id in mdata$id){
    cat(paste0("Processing ", id, "\n"))
    trips[[id]] <- get_trip_times(gtfs, id)
}

# TODO: por que length(trips) == length(mdata$id) + 1?

#get_trip_times(gtfs, "149-1")

mdata$trips <- 0
mdata$time <- 0

total <- length(mdata$id)

for(id in 1:total){
    cat(paste0("Processing ", id, "/", total, "\n"))
    mytrips <- trips[[mdata$id[id]]]
    
    if(!is.null(mytrips)){
        mdata$trips[id] <- dim(mytrips)[1] # number of trips
        mdata$time[id] <- mean(mytrips$trip_time) # average time of all trips
    }    
}

mdata$time <- mdata$time %>% set_units(s) %>% set_units(h)

mdata <- mdata %>%
    mutate(speed = length / time) %>%
    filter(trips > 0) %>%
    filter(speed < set_units(100, km/h))


hist(mdata$speed)

plot(mdata[,"speed"])

hist(mdata$trips)

plot(mdata[,"trips"])


##########################################################

# basic version: do not create points, only extract from the line
getGPSpoints <- function(line, mytraveltime, delay){
    points_in_time = mytraveltime / delay
    mylength <- line %>% st_length()
    myunit <- units::deparse_unit(mylength)
    distance_between_points <- mylength %>% set_units(km) / points_in_time
    
    coords <- st_coordinates(line)[,-3]

    result <- coords[1,]
    
    accumulated_dist = set_units(0, m)
    
    for(i in 1:(dim(coords)[1]-1)){
        l1 = coords[i,]
        l2 = coords[i + 1,]
        
        pl1 = st_point(l1)
        pl2 = st_point(l2)
    
        dist <- st_distance(pl1, pl2) %>% as.numeric() %>% set_units(myunit, mode = "standard")
        
        if(accumulated_dist + dist > distance_between_points){
            # compute where in the line the point should be created
            # currently it creates in the end, which is not the best solution
            result <- rbind(result, l1)
            accumulated_dist <- set_units(0, m)
        }
        else{
            accumulated_dist <- accumulated_dist + dist
        }
    }
    
    result <- rbind(result, coords[dim(coords)[1],])
    rownames(result) <- NULL
    return(result)
}

# improved version: interpolate points whenever needed
getGPSpoints <- function(line, mytraveltime, delay){
    points_in_time = mytraveltime / delay
    mylength <- line %>% st_length()
    myunit <- units::deparse_unit(mylength)
    distance_between_points <- mylength %>% set_units(km) / points_in_time
    
    coords <- st_coordinates(line)[,-3]
    
    result <- coords[1,]
    accumulated_dist = set_units(0, m)
    
    for(i in 1:(dim(coords)[1]-1)){
        l1 <- coords[i, ]
        l2 <- coords[i + 1, ]
        
        pl1 <- st_point(l1)
        pl2 <- st_point(l2)
        
        dist <- st_distance(pl1, pl2) %>% as.numeric() %>% set_units(myunit, mode = "standard")

        if(accumulated_dist + dist > distance_between_points){
            percentage <- (distance_between_points - accumulated_dist) / dist
            
            dx <- (l1[1] - l2[1]) * percentage %>% as.numeric()
            dy <- (l1[2] - l2[2]) * percentage %>% as.numeric()
            mypoint <- c(l1[1] + dx, l1[2] + dy) %>% st_point
            
            result <- rbind(result, mypoint)
            
            # supposing that two gtfs points will produce at most one GPS point
            accumulated_dist <- accumulated_dist + dist - distance_between_points
        }
        else{
            accumulated_dist <- accumulated_dist + dist
        }
    }
    
    result <- rbind(result, coords[dim(coords)[1],])
    rownames(result) <- NULL
    return(result)
}

# figures to put in the presentation

mydelay <- 25
mydelaymin <- set_units(mydelay, minutes)
id <- 2 # id 1 to 11 are similar

result <- getGPSpoints(mdata$geometry[id], mdata$time[id], mydelaymin)

plot(mdata$geometry[id])
plot(st_multipoint(result), col="blue", pch=15, add=T)

getGPStable <- function(mdata, mdata_id, trips, trip_id, mydelay){
    mydelaymin <- set_units(mydelay, minutes)
    
    result <- getGPSpoints(mdata$geometry[mdata_id], mdata$time[mdata_id], mydelaymin)

    first_trip <- trips[[mdata$id[mdata_id]]][trip_id, ]
    
    myresult <- result %>% as_tibble()
    myresult$trip_id <- first_trip$trip_id
    
    myresult$stop_id <- "moving"
    myresult$stop_id[1] <- first_trip$stop_id
    
    first_time <- as.numeric(first_trip$start)
    end_time <- as.numeric(first_trip$finish)
    steps <- dim(myresult)[1] - 2
    last_time <- first_time + mydelay * 60 * steps
    times <- c(seq(from = first_time, to = last_time, by = mydelay * 60), end_time)
    myresult$departure_time <- hms::hms(times)
    
    myresult$stop_sequence <- c(first_trip$stop_sequence, paste0(first_trip$stop_sequence, ".", 1:(dim(myresult)[1] - 1)))
    
    myresult %>% select(trip_id, stop_id, departure_time, lat = Y, long = X, stop_sequence)
}

getGPStable(mdata, 1, trips, 1, 25)
getGPStable(mdata, 2, trips, 1, 5)

result <- tibble()

maxi <- dim(mdata)[1]

for(i in 1:maxi)
{
    maxj <- dim(trips[[mdata$id[i]]])[1]
    cat(paste0("Processing ", i, "/", maxi, " with ", maxj, "trips\n"))
    for(j in 1:maxj)
    {
        result <- rbind(result, getGPStable(mdata, i, trips, j, 25))
    }
}


# TODO:
# lat e long nao sao em graus, eles estao em metros - GPS eh em graus

}
