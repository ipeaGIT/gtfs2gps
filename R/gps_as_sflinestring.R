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
  
  # get row potion of each stop
  id0 <- c(1, which(!is.na(dt$stop_sequence)))
  
  # get row position of consecutive stops
  id1 <- c(id0[-1], nrow(dt))
  
  # create a data table grouping ids by unique intervals
  # Here we create a data.table indicating what are all the point ids in each interval
  list_ids <- data.table::data.table(
    interval = rep(seq_along(id0), id1 - id0 + 1),
    id = unlist(lapply(seq_along(id0), function(i) id0[i]:id1[i]))
  )
  
  # add interval code to GPS
  dt[list_ids, on = "id", interval_id := i.interval]
  
  # rename columns
  data.table::setnames(dt,"stop_id","from_stop_id")
  
  ## Each stop is the start of an interval and the end of another one.
  ## So we we need to duplicate each stop to make sure every interval has a unique start and end point  
  
  # get unique valid stops (extra spatial points)
  dt1 <- data.table::copy(dt)[, .SD[1], by = .(trip_id, interval_id, trip_number)]
  
  # rename columns
  data.table::setnames(dt1, "from_stop_id", "to_stop_id")
  #dt1 <- data.table::setcolorder(dt1, names(dt))
  
  # recode their unique id's so they fall and the end of each interval
  dt1[, c("id", "interval_id") := list(id - 0.1, interval_id - 1)] 
  
  # add extra points in valid_id's of the GPS data
  dt2 <- data.table::rbindlist(l = list(dt, dt1), use.names = TRUE, fill = TRUE)[order(id)]
  
  # create unique id for each unique combination of interval_id & trip_id & trip_number
  dt2[, grp := .GRP, by = .(interval_id, trip_id, trip_number)]
  
  # dt2[, .N, by = grp] # number of observations in each grp
  
  moreThanOne <- which(as.vector(table(dt2$grp)) != 1)
  
  dt2 <- dt2[grp %in% moreThanOne, ]
  dt2[, timestamp := data.table::as.ITime(timestamp)]
  
  dt2[, to_stop_id := to_stop_id[.N], by = grp]
  
  ## convert to linestring
  gps_sf <- sfheaders::sf_linestring(obj = dt2, 
                                     x = 'shape_pt_lon',
                                     y = 'shape_pt_lat',
                                     linestring_id = 'grp',
                                     keep = TRUE)
  gps_sf <- sf::st_set_crs(gps_sf, crs)
  
  # calculate legnth of each segment
  setDT(gps_sf)[, dist := sf::st_length(geometry)]
  
  # edit columns
  gps_sf[, grp := NULL]
  gps_sf[, cumdist := NULL]
  gps_sf[, cumtime := NULL]
  gps_sf <- st_sf(gps_sf)

  # order columns "stop_id" <> "to_stop_id"
  colsToStop <- names(gps_sf)[1:which(names(gps_sf) %in% "from_stop_id")]
  colsFromStop <- names(gps_sf)[(which(names(gps_sf) %in% "from_stop_id") + 1):(which(names(gps_sf) %in% "to_stop_id") - 1)]
  colsNewnames <- c(colsToStop, "to_stop_id", colsFromStop)
  gps_sf <- gps_sf[colsNewnames]
  
  return(gps_sf)
}
