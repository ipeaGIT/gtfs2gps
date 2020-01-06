#' @title Convert GTFS shapes to simple feature object
#' 
#' @description Convert a GTFS shapes data loaded using gtfs2gps::read_gtf()
#' into a line simple feature (sf).
#' @param gtfs A GTFS data.
#' @param crs The coordinate reference system represented as an EPSG code.
#' The default value is 4326 (latlong WGS84)
#' @export
#' @examples \donttest{
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#' poa_sf <- gtfs_shapes_as_sf(poa)
#' plot(sf::st_geometry(poa_sf), lwd = 2)
#' }
gtfs_shapes_as_sf <- function(gtfs, crs = 4326){
  temp_shapes <- gtfs$shapes[,
                          {
                            geometry <- sf::st_linestring(x = matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2))
                            geometry <- sf::st_sfc(geometry)
                            geometry <- sf::st_sf(geometry = geometry)
                          }
                          , by = shape_id
                          ]

  temp_shapes <- sf::st_as_sf(temp_shapes, crs = crs)
  setDT(temp_shapes)[, length := sf::st_length(geometry) %>% units::set_units("km") ] 
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
  gtfs$stops %>%
    sf::st_as_sf(coords = c('stop_lon', 'stop_lat'), agr = "identity", crs = crs)
}
