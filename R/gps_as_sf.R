
#' @title Convert GPS-like data.table to a Simple Feature object
#' @description Convert a GPS data stored in a data.table into a Simple Feature.
#' @param gps A data.table with timestamp data.
#' @param crs A Coordinate Reference System. The default value is 4326 (latlong WGS84)
#' @return A simple feature (sf) object.
#' @export
#' @examples
#' library(dplyr)
#' 
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' subset <- filter_by_shape_id(poa, "T2-1") %>%
#'   filter_single_trip()
#' 
#' poa_gps <- gtfs2gps(subset)
#' poa_gps_sf <- gps_as_sf(poa_gps)
gps_as_sf <- function(gps, crs = 4326){
  # convert to sf
  temp_gps <- sfheaders::sf_multipoint(gps, x = "shape_pt_lon", y = "shape_pt_lat",
                                       multipoint_id = "shape_id", keep = TRUE)

  # add projection
  sf::st_crs(temp_gps) <- crs
  return(temp_gps)
}
