
#' @title Convert GTFS shapes to simple feature
#' @description Convert a GTFS shapes data loaded using gtfs2gps::read_gtf()
#' into a line simple feature (sf).
#' @param gtfs A GTFS data.
#' @param crs The coordinate reference system represented as an EPSG code.
#' The default value is 4326 (latlong WGS84)
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#' poa_sf <- gtfs_shapes_as_sf(poa)
#' plot(sf::st_geometry(poa_sf))
gtfs_shapes_as_sf <- function(gtfs, crs = 4326){
  gtfs$shapes %>%
    split(.$shape_id) %>%
    purrr::map(~dplyr::select(., shape_pt_lon, shape_pt_lat) %>%
               as.matrix %>%
               sf::st_linestring()) %>%
    sf::st_sfc(crs = crs)
}