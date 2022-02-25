#' @title Converts a GPS-like data.table to a LineString Simple Feature (sf) object
#'
#' @description Every interval of GPS data points between stops for each trip_id is
#'  converted into a linestring segment. The output assumes constant average speed 
#'  between consecutive stops.
#'
#' @param gps A data.table with timestamp data.
#' @param crs A Coordinate Reference System. The default value is 4326 (latlong WGS84).
#' @return A simple feature (sf) object with LineString data.
#' @export
#' @examples
#' library(gtfs2gps)
#' library(dplyr)
#' 
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' poa_subset <- filter_by_shape_id(poa, c("T2-1", "A141-1")) %>%
#'   filter_single_trip()
#' 
#' poa_gps <- gtfs2gps(poa_subset)
#' 
#' poa_gps_sf <- gps_as_sflinestring(poa_gps)
gps_as_sflinestring  <- function(gps, crs = 4326){
  
  if(is.character(gps)){
    dt <- data.table::fread(gps, colClasses = list(character = c("id", "shape_id", "trip_id", "stop_id")))
  } else {
    dt <- gps
  }
  
  # create new row ids
  dt[, id := .I]
  
  ## stop intervals
  ## each pair of consecutive stops is called a 'unique interval'
  
  # get stop_ids that is duplicated (number == 2)
  dt[,numbers :=  .N,by = .(shape_id,trip_id,stop_id,stop_sequence,trip_number)]
  dt[is.na(stop_sequence),numbers := NA]
  
  fir_id <- data.table::copy(dt)[!is.na(stop_sequence) & numbers == 2
                                 ,.SD[1]
                                 ,by = .(shape_id,trip_id,stop_id,stop_sequence,trip_number)]
  fir_id[id == 1 & !is.na(stop_id),speed := dt[id == 3,]$speed]
  
  sec_id <- data.table::copy(dt)[!is.na(stop_sequence) & numbers == 2
                                 ,.SD[2]
                                 ,by = .(shape_id,trip_id,stop_id,stop_sequence,trip_number)]
  sec_id[id == 2 & !is.na(stop_id),speed := dt[id == 3,]$speed]
  
  # get id / stop / timestamp position of consecutive stops
  id0 <- c(1,sec_id$id)
  stop0 <- c(sec_id$stop_id[1],sec_id$stop_id)
  timestamp0 <- c(sec_id$timestamp[1],sec_id$timestamp)
  
  id1 <- c(fir_id$id,nrow(dt))
  stop1 <- c(fir_id$stop_id,tail(fir_id$stop_id,1))
  timestamp1 <- c(fir_id$timestamp,tail(fir_id$timestamp,1))
  # head(id0); head(id1)
  # tail(id0); tail(id1)
  # head(stop0);head(stop1)
  # head(timestamp0);head(timestamp1)
  # tail(stop0);tail(stop1)
  # tail(timestamp0);tail(timestamp1)
  
  # create a data table grouping ids by unique intervals
  # Here we create a data.table indicating what are all the point ids in each interval
  
  list_ids <- data.table::data.table(
    from_stop_id = rep(stop0, id1 - id0 + 1),
    to_stop_id = rep(stop1, id1 - id0 + 1),
    from_timestamp = rep(timestamp0, id1 - id0 + 1),
    to_timestamp = rep(timestamp1, id1 - id0 + 1),
    id = unlist(lapply(seq_along(id0), function(i) id0[i]:id1[i])),
    trip_number = dt$trip_number,
    shape_id = dt$shape_id
  )
  
  list_ids[, interval_status := .GRP, by = c("shape_id","from_stop_id","to_stop_id","trip_number")]
  my_f <- function(a){
    b <- c(a[1],head(a,-1))
    c <- a - b
    d <- c(1,which(c!=0),length(c)+1)
    e <- d[-1]-d[-length(d)]
    g <- rep(1:length(e),e)
    return(g)
  }
  list_ids[,interval_status := my_f(interval_status), by = c("shape_id","trip_number")]
  list_ids[, from_stop_id := data.table::fifelse(interval_status == min(interval_status)
                                                 ,"-",from_stop_id), by = c("shape_id","trip_number")]
  list_ids[, to_stop_id := data.table::fifelse(interval_status == max(interval_status)
                                               ,"-",to_stop_id), by = c("shape_id","trip_number")]
  list_ids[, from_timestamp := data.table::fifelse(interval_status == min(interval_status)
                                                   ,as.ITime(NA),from_timestamp), by = c("shape_id","trip_number")]
  list_ids[, to_timestamp := data.table::fifelse(interval_status == max(interval_status)
                                                 ,as.ITime(NA),to_timestamp), by = c("shape_id","trip_number")]
  
  # add interval code to GPS
  dt[list_ids, on = c("id","trip_number","shape_id"),`:=`(interval_id = i.interval_status
                                                          ,from_stop_id = i.from_stop_id
                                                          ,to_stop_id = i.to_stop_id
                                                          ,from_timestamp = i.from_timestamp
                                                          ,to_timestamp = i.to_timestamp
  )]
  
  # add info on timestamps and update interval_id
  dt[,N_intervals := .N,by = .(interval_id,trip_number,trip_id,shape_id)]
  dt <- dt[N_intervals > 1, ]
  dt[,N_intervals := NULL]
  dt[,numbers := NULL]
  dt[, from_timestamp := data.table::as.ITime(from_timestamp)]
  dt[, to_timestamp := data.table::as.ITime(to_timestamp)]
  dt[, interval_id := paste0(interval_id,"_",shape_id,"_",trip_number,"_",to_stop_id,"_",from_stop_id)]
  
  ## convert to linestring
  gps_sf <- sfheaders::sf_linestring(obj = dt, 
                                     x = 'shape_pt_lon',
                                     y = 'shape_pt_lat',
                                     linestring_id = 'interval_id',
                                     keep = TRUE)
  gps_sf <- sf::st_set_crs(gps_sf, crs) # crs=4329
  
  # calculate legnth of each segment
  data.table::setDT(gps_sf)[, dist := sf::st_length(geometry)]
  gps_sf$interval_id <- NULL
  
  # add time / speed info
  gps_sf[,time := to_timestamp - from_timestamp]
  gps_sf[,time := units::set_units(as.numeric(time),"s")]
  gps_sf[,speed := units::set_units(dist/time,"km/h")]
  
  # edit columns
  gps_sf[, cumdist := cumsum(dist), by = c("shape_id","trip_id","trip_number")]
  gps_sf[, cumtime := cumsum(time), by = c("shape_id","trip_id","trip_number")]
  gps_sf[, time := NULL]
  gps_sf[, stop_id := NULL]
  gps_sf[, interval_id := NULL]
  gps_sf <- sf::st_sf(gps_sf)
  
  
  return(gps_sf)
}
