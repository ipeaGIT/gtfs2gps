
library(dplyr)
library(sf)

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

# id, length, geometry
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
        id = mtrip_id,
        start = stop_times$arrival_time_hms[start],
        stop_id = stop_times$stop_id[1],
        stop_sequence = stop_times$stop_sequence[1],
        finish = stop_times$departure_time_hms[finish],
        trip_time = trip_times
    )
}

trips = tibble()

for(id in mdata$id){
    cat(paste0("Processing ", id, "\n"))
    trips <- rbind(trips, get_trip_times(gtfs, id))
}

poa <- dplyr::inner_join(trips, mdata, by = "id")


poa$time <- poa$trip_time %>% as.numeric() %>% set_units(s) %>% set_units(h)

poa <- poa %>% mutate(vel = length / time) %>%
    select(-trip_time, -stop_sequence)


require(lubridate)

# improved version: interpolate points whenever needed
breakLineByHour <- function(line, time_table){
    coords <- st_coordinates(line)[,-3]
    mylength <- line %>% st_length()
    
    myunit <- units::deparse_unit(mylength)

    time_table$geometry <- line
    
    current_break = 1
    
    distance_between_points <- time_table$dist[1]
    result <- coords[1,] %>% st_point()
    
    result <- rbind(result, coords[2,] %>% st_point())
    
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
            
            result <- rbind(result, mypoint) %>% st_linestring()
            time_table$geometry[current_break] <- result 
            result <- mypoint

            accumulated_dist <- (1 - (percentage %>% as.numeric())) * (dist %>% as.numeric()) %>% set_units(myunit, mode = "standard")
            current_break <- current_break + 1            
            distance_between_points <- time_table$dist[current_break]
        }
        else{
            accumulated_dist <- accumulated_dist + dist
            result <- rbind(result, pl2)
        }
    }
    
    if(current_break < dim(time_table)[1]){
        result <- rbind(result, coords[dim(coords)[1],]) %>% st_linestring()
        time_table$geometry[current_break] <- result 
    }
    else{
        # TODO: due to some rounding issue, there are a few remaining points
        # this situation is rare, but might happen
    }
    
    return(time_table)
}

# breaks a time interval into several time intervals splitting by hours
# for example 
# [3:59, 5:10] -> [3:59, 4:00, 5:00, 5:10]
# [4:00, 5:10] -> [4:00, 5:00, 5:10]
# [3:11, 4:15] -> [3:11, 4:00, 4:15]
# [3:11, 3:15] -> [3:11, 3:15]
breakHours <- function(start, finish){
    start_hour <- hour(start)
    finish_hour <- hour(finish)
    
    if(start_hour == finish_hour) return(c(start, finish))
    
    additional_times <- (finish_hour - start_hour) %% 24
    
    if(minute(finish) == 0){
        additional_times <- additional_times - 1
        if(additional_times == 0) return(c(start, finish))
    } 
    myhours <- (start_hour + 1):(start_hour + additional_times)
    c(start, hms::hms(hours = myhours), finish)
}

poa_splitted <- tibble::tibble()

for(i in 1:dim(poa)[1]){
    cat(paste0("Processing ", i, "/", dim(poa)[1], "\n"))
    h <- breakHours(poa$start[i], poa$finish[i])
    
    result <- tibble::tibble(
        start = h[-length(h)],
        finish = h[-1],
        dt = finish - start,
        id = poa$id[i],
        trip_id = poa$trip_id[i],
        dist = poa$length[i] * (as.numeric(dt) / as.numeric(poa$finish[i] - poa$start[i]))
    )
    
    splitted <- breakLineByHour(poa$geometry[i], result)
    poa_splitted = rbind(poa_splitted, splitted)
}

mpoa <- st_as_sf(poa_splitted)

plot(mpoa %>% st_geometry)

## IMPORTANT: WRITE_SF DOES NOT WRITE DATE/TIME OBJECTS. IT IGNORES THEM
# IT IS NECESSARY TO MANUALLY CONVERT THEM TO STRINGS.

mpoa$start <- as.character(mpoa$start)
mpoa$finish <- as.character(mpoa$finish)
mpoa$dt <- as.character(mpoa$dt)

write_sf(mpoa, "poa-splitted.shp")

# TO RECOVER POA-SPLITTED.SHP:
recover_mpoa <- function(){
    read_sf("mpoa-roads-splitted.shp") %>%
        mutate(
            start = hms::as.hms(start),
            finish = hms::as.hms(finish),
            dt = as.numeric(dt) %>% set_units(s),
            dist = set_units(dist, "km")
        )
}


betweenHours <- function(data, h1, h2){
    hms1 <- hms::hms(hours = h1)
    hms2 <- hms::hms(hours = h2)
    return(dplyr::filter(data, start >= hms1 & start < hms2))
}

mpoa <- mpoa %>%
    mutate(vel = dist / (as.numeric(dt) / 3600))

travelsByHour      <- sapply(1:24, function(v) dim (betweenHours(mpoa, v, v + 1))[1])
kmByHour           <- sapply(1:24, function(v) sum (betweenHours(mpoa, v, v + 1)$dist))
velByHour          <- sapply(1:24, function(v) mean(betweenHours(mpoa, v, v + 1)$vel))
minTravelledByHour <- sapply(1:24, function(v) sum (betweenHours(mpoa, v, v + 1)$dt)/60)


bus1 <- vein::age_hdv(30, agemax = 4)
lef <- vein::ef_cetesb("COd", "UB")

emisByHour <- sapply(1:24, function(v){
    vein::emis(
        veh = bus1,
        lkm = sum (betweenHours(mpoa, v, v + 1)$dist),
        speed = 40, # changes in speed do not change emission
        ef = lef
    ) %>% sum(na.rm = TRUE)
})

plot(emisByHour, type="l", lwd=2)

par(mfrow=c(2,2), mar=c(2.1, 4.1, 1.1, 0.3))
plot(travelsByHour, type="l", lwd=2)
plot(kmByHour, type="l", lwd=2)
plot(velByHour, type="l", lwd=2)
plot(emisByHour, type="l", lwd=2)
