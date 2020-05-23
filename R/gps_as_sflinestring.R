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
    dt <- data.table::fread(gps)
  } else {
    dt <- gps
  }
  
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
  list_ids <- lapply(seq_along(id0), function(i){data.table::data.table(interval = i, id = (id0[i]:id1[i]))}) %>%
      data.table::rbindlist()
    
  # add interval code to GPS
  dt[list_ids, on = "id", interval_id := i.interval]

  ## Each stop is the start of an interval and the end of another one.
  ## So we we need to duplicate each stop to make sure every interval has a unique start and end point  
  
  # get unique valid stops (extra spatial points)
  dt1 <- dt[, .SD[1], by = .(trip_id, interval_id)]
  
  # reorder columns
  dt1 <- data.table::setcolorder(dt1, names(dt))
  
  # recode their  unique id's so they fall and the end of each interval 
  dt1[, c("id", "interval_id") := list(id - 0.1, interval_id - 1)] 
  
  # add extra points in valid_id's of the GPS data
  dt2 <- data.table::rbindlist(list(dt, dt1))[order(id)]
  
  # create unique id for each unique combinarion of interval_id & trip_id
  dt2[, grp := .GRP, by = .(interval_id, trip_id)]

  dt2[, .N, by = grp] # number of observations in each grp
  
  moreThanOne <- which(as.vector(table(dt2$grp)) != 1)
  
  dt2 <- dt2[grp %in% moreThanOne, ]

  ## convert to linestring
  gps_sf <- sfheaders::sf_linestring(obj = dt2, 
                                              x = 'shape_pt_lon',
                                              y = 'shape_pt_lat',
                                              linestring_id = 'grp',
                                              keep = TRUE) %>% sf::st_set_crs(crs)

  # edit columns
  gps_sf$departure_time <- data.table::as.ITime(gps_sf$departure_time)
  gps_sf$dist <- sf::st_length(gps_sf$geometry)
  gps_sf$grp <- NULL

  return(gps_sf)
}
