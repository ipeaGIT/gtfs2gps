
require(Rcpp)

cppFunction("Rcpp::DataFrame cppGetGPSpoints(Rcpp::NumericMatrix& data, const double distance_between_points){
    Rcpp::NumericVector result_x;
    Rcpp::NumericVector result_y;

    const int nrow = data.nrow();
    double accumulated_dist = 0;

    result_x.push_back(data[0]);
    result_y.push_back(data[nrow]);

    for(int i = 1; i < nrow - 1; i++){
        const double x0 = data[i - 1];
        const double x1 = data[i];
        const double y0 = data[i + nrow - 1];
        const double y1 = data[i + nrow];

        const double dist = sqrt(pow(x1 - x0, 2) + pow(y1 - y0, 2));

        if(accumulated_dist + dist > distance_between_points){
            const double percentage = (distance_between_points - accumulated_dist) / dist;
        
            const double dx = (x1 - x0) * percentage;
            const double dy = (y1 - y0) * percentage;
            const double mypointx = x0 + dx;
            const double mypointy = y0 + dy;
  
            result_x.push_back(mypointx);
            result_y.push_back(mypointy);

            // supposing that two gtfs points will produce at most one GPS point
            accumulated_dist = accumulated_dist + dist - distance_between_points;
        }
        else{
            accumulated_dist = accumulated_dist + dist;
        }
    }

    result_x.push_back(data[nrow - 1]);
    result_y.push_back(data[nrow * 2 - 1]);
    
    Rcpp::DataFrame result = 
    Rcpp::DataFrame::create(Rcpp::Named(\"x\") = result_x,
                        Rcpp::Named(\"y\") = result_y);

    return result;
}")

getGPSpoints <- function(line, mytraveltime, delay){
    points_in_time = mytraveltime / delay
    mylength <- line %>% st_length()
    myunit <- units::deparse_unit(mylength)
    distance_between_points <- (mylength %>% set_units(km) / points_in_time) %>%
        set_units(m)
    
    coords <- st_coordinates(line)[,-3]
    
    result <- gps_points(coords, distance_between_points) %>% as.matrix(col=2)
    rownames(result) <- NULL
    return(result)
}

getGPStable <- function(mdata, mdata_id, trips, trip_id, mydelay){
    mydelaymin <- set_units(mydelay, minutes)
    
    result <- getGPSpoints(mdata$geometry[mdata_id], mdata$time[mdata_id], mydelaymin)
    
    first_trip <- dplyr::filter(trips, id == mdata$id[mdata_id])[trip_id, ]
    
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
    
    myresult %>% select(trip_id, stop_id, departure_time, lat = y, long = x, stop_sequence)
}

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

result <- tibble()

maxi <- dim(mdata)[1]

for(i in 1:maxi)
{
    maxj <- dim(filter(trips, id == mdata$id[i]))[1]
    cat(paste0("Processing ", i, "/", maxi, " with ", maxj, " trips\n"))
    for(j in 1:maxj)
    {
        result <- rbind(result, getGPStable(mdata, i, trips, j, 25))
    }
}

write.csv(result, "gps-poa.csv")
