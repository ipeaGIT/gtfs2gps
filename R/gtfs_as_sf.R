
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
#' plot(poa_sf["id"], lwd = 2)
gtfs_shapes_as_sf <- function(gtfs, crs = 4326){
  mysplit <- gtfs$shapes %>%
    split(.$shape_id)
  
  lines <- mysplit %>%
    purrr::map(~dplyr::select(., shape_pt_lon, shape_pt_lat) %>%
               as.matrix %>%
               sf::st_linestring()) %>%
    sf::st_sfc()
  
  data.frame(
    geom = lines,
    id = names(mysplit),
    length = lines %>% sf::st_length() %>% units::set_units(km),
    stringsAsFactors = FALSE) %>%
    sf::st_sf(crs = crs)
}

#' @title Convert GTFS stops to simple feature
#' @description Convert a GTFS stops data loaded using gtfs2gps::read_gtf()
#' into a point simple feature (sf).
#' @param gtfs A GTFS data.
#' @param crs The coordinate reference system represented as an EPSG code.
#' The default value is 4326 (latlong WGS84)
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#' poa_shapes <- gtfs_shapes_as_sf(poa)
#' poa_stops <- gtfs_stops_as_sf(poa)
#' plot(st_geometry(poa_shapes), lwd = 2)
#' plot(st_geometry(poa_stops), pch = 20, col ="blue", add = TRUE)
gtfs_stops_as_sf <- function(gtfs, crs = 4326){
  gtfs$stops %>%
    sf::st_as_sf(coords = c('stop_lon', 'stop_lat'), agr = "identity", crs = crs)
}