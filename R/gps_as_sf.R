
#' @title Convert GPS-like data.table to a Simple Feature object
#' @description Convert a GPS data stored in a data.table into a Simple Feature.
#' @param gps A data.table with timestamp data.
#' @param crs A Coordinate Reference System. The default value is 4326 (latlong WGS84)
#' @export
gps_as_sf <- function(gps, crs = 4326){
  sf::st_as_sf(x = gps, coords = c("shape_pt_lon", "shape_pt_lat"), crs = crs)
}
