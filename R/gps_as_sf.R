
#' @title Convert GPS data to a Simple Feature
#' @description Convert a GPS data stored in a data.table into a Simple Feature
#' @param gps A data.table with GPS data.
#' @param crs A Coordinate Reference System. The default value is 4326.
#' @export
gps_as_sf <- function(gps, crs = 4326){
  sf::st_as_sf(x = gps, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326)
}
