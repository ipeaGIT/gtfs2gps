#' @title Converts a GPS-like data.table to a MultiLineString Simple Feature (sf) object
#'
#' @description Every interval of GPS data points of constant speed for each trip_id is
#'  converted into a linestring segment.
#'
#' @param input_file A data.table with timestamp data.
#' @param crs A Coordinate Reference System. The default value is 4326 (latlong WGS84).
#' @return A simple feature (sf) object with MultiLineString data.
#' 
#' @export
#' @examples \donttest{
#' 
#' library(gtfs2gps)
#' library(dplyr)
#' 
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' poa_subset <- filter_by_shape_id(poa, c("T2-1", "A141-1")) %>%
#'   filter_single_trip()
#' 
#' poa_gps <- gtfs2gps(poa_subset)
#' poa_gps_sf <- gps2sf_linestring(poa_gps)
#' plot(poa_gps_sf$geometry)
#'}
#'
gps2sf_linestring  <- function(input_file, crs = 4326){
  
  # input_file <- poa_gps
  
  if(is.character(input_file)){
    dt <- data.table::fread(input_file)} else {
    dt <- input_file
  }
  
  
  ### iteration over all trip_id's
  
  
  # create new row ids
  dt[, id := .I]
  
  ## stop intervals
  ## each pair of consecutive stops is called a 'unique interval'
  
  # get row potion of each stop
  id0 <- c(1, which(!is.na(dt$stop_sequence)))
  
  # get row position of consecutive stops
  id1 <- c(id0[-1], nrow(dt))
  

  
  # create a data table grouping ids by unique intervals
    # # Here we create a data.table indicating what are all the point ids in each interval
    list_ids <- lapply(seq_along(id0),function(i){data.table::data.table(interval = i, id = (id0[i]:id1[i]))}) %>%
      data.table::rbindlist()
    
  # add interval code to GPS
  dt[list_ids, on = "id", interval_id := i.interval]
  
  

  ## Each stop is the start of an interval and the end of another one.
  ## So we we need to duplicate each stop to make sure every interval has a unique start and end point  
  
    # get unique valid stops (extra spatial points)
    dt1 <- dt[,.SD[1],by = .(trip_id, interval_id)]
    
    # reorder columns
    dt1 <- data.table::setcolorder(dt1,names(dt))
    
    # recode their  unique id's so they fall and the end of each interval 
    dt1[, c("id","interval_id") := list(id - 0.1, interval_id - 1)] 
    
    # add extra points in valid_id's of the GPS data
    dt2 <- data.table::rbindlist(list(dt,dt1))[order(id)]
    
    # create unique id for each unique combinarion of interval_id & trip_id
    dt2[, grp := .GRP, by = .(interval_id,trip_id) ]
  

 
  #### 1/2 solucao sfheaders puro
    
  tictoc::tic()
  gps_sf2 <- sfheaders::sf_linestring(obj=dt2, 
                                              x='shape_pt_lon',
                                              y='shape_pt_lat',
                                              linestring_id = 'grp',
                                              keep=TRUE) %>% sf::st_set_crs( crs )
  
  
  gps_sf2 <- sf::st_make_valid(gps_sf2)
  
  gps_sf2$departure_time <- data.table::as.ITime(gps_sf2$departure_time)
  gps_sf2$dist <- sf::st_length(gps_sf2$geometry)
  gps_sf2 <- gps_sf2[as.numeric(gps_sf2$dist) > 0,]
  gps_sf2$grp <- NULL
  gps_sf2$grp <- NULL
  
  
  
  tictoc::toc()
  
  
  
  #### 2/2 solucao function
  
  tictoc::tic()
  # function to convert to multilnestring
  flines <- function(long,lat){
    exp <- matrix(c(long,lat),ncol = 2) %>% sfheaders::sf_linestring()
    return(exp$geometry)
  }
  
  gps_sf <- dt2[,geometry := list(flines(shape_pt_lon,shape_pt_lat)),by = grp][, .SD[1], by = grp ] %>% 
    sf::st_as_sf() %>% sf::st_set_crs( crs )
  
  #dt3$geometry <- sf::st_sf(geometry = geom,crs = 4326)
  # as.Itime
  gps_sf$departure_time <- data.table::as.ITime(gps_sf$departure_time)
  gps_sf$dist <- sf::st_length(gps_sf$geometry)
  gps_sf <- gps_sf[as.numeric(gps_sf$dist) > 0,]
  gps_sf$grp <- NULL
  gps_sf$id <- NULL
  gps_sf$shape_pt_lat <- NULL
  gps_sf$shape_pt_lon <- NULL
  
  tictoc::toc()

  
  return(gps_sf)
}


# 
# setdiff(names(gps_sf), names(gps_sf2))
# setdiff(names(gps_sf2), names(gps_sf))
# 
# setcolorder(gps_sf, names(gps_sf2))
# 
# identical(gps_sf, gps_sf2)
# 
# 
# identical(gps_sf$trip_id, gps_sf2$trip_id)
# identical(gps_sf$route_type, gps_sf2$route_type)
# identical(gps_sf$departure_time, gps_sf2$departure_time)
# identical(gps_sf$stop_id, gps_sf2$stop_id)
# identical(gps_sf$stop_sequence, gps_sf2$stop_sequence)
# identical(gps_sf$dist, gps_sf2$dist)
# identical(gps_sf$interval_id, gps_sf2$interval_id)
# identical(gps_sf$geometry, gps_sf2$geometry)
# 
# st_crs(gps_sf) == st_crs(gps_sf2)
# 
# 
# library(ggplot2)
# 
# 
# ggplot() + geom_sf(data=dt2, aes(color=as.factor(speed)))
# 
# 
# dt3 <- copy(dt2)
# 
# identical(dt2, dt3)

