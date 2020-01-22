#' @title Convert GTFS shapes to simple feature object
#' 
#' @description Convert a GTFS shapes data loaded using gtfs2gps::read_gtf()
#' into a line simple feature (sf).
#' @param gtfs A GTFS data.
#' @param crs The coordinate reference system represented as an EPSG code.
#' The default value is 4326 (latlong WGS84)
#' @export
#' @examples \donttest{
#' poa <- read_gtfs(system.file("extdata/saopaulo.zip", package="gtfs2gps"))
#' poa_sf <- gtfs_shapes_as_sf(poa)
#' plot(sf::st_geometry(poa_sf), lwd = 2)
#' }
gtfs_shapes_as_sf <- function(gtfs, crs = 4326){
  # sort data
  temp_shapes <- data.table::setDT(gtfs$shapes)[order(shape_id, shape_pt_sequence)]
  
  # convert to sf
  temp_shapes <- sfheaders::sf_linestring(temp_shapes, x = "shape_pt_lon" , y = "shape_pt_lat", linestring_id = "shape_id")
  
  # add projection
  sf::st_crs(temp_shapes) <- crs
  
  # calculate distances
  data.table::setDT(temp_shapes)[, length := sf::st_length(geometry) %>% units::set_units("km") ] 
  
  # back to sf
  temp_shapes <- sf::st_sf(temp_shapes)
  return(temp_shapes)
}

#' @title Convert GTFS stops to simple feature object
#' @description Convert a GTFS stops data loaded using gtfs2gps::read_gtf()
#' into a point simple feature (sf).
#' @param gtfs A GTFS data.
#' @param crs The coordinate reference system represented as an EPSG code.
#' The default value is 4326 (latlong WGS84)
#' @export
#' @examples \donttest{
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#' poa_shapes <- gtfs_shapes_as_sf(poa)
#' poa_stops <- gtfs_stops_as_sf(poa)
#' plot(sf::st_geometry(poa_shapes), lwd = 2)
#' plot(sf::st_geometry(poa_stops), pch = 20, col ="blue", add = TRUE)
#' }
gtfs_stops_as_sf <- function(gtfs, crs = 4326){
  temp_stops_sf <- sfheaders::sf_point(gtfs$stops, x = "stop_lon", y="stop_lat", keep = T)
  sf::st_crs(temp_stops_sf) <- crs
  return(temp_stops_sf)
}
