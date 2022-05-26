#' @title Convert GPS-like data.table to a Simple Feature points object
#' 
#' @description Convert a GPS data stored in a data.table into Simple Feature points.
#'
#' @param gps A data.table with timestamp data.
#' @param crs A Coordinate Reference System. The default value is 4326 (latlong WGS84).
#' @return A simple feature (sf) object with point data.
#' @export
#' @examples
#' library(gtfs2gps)
#' library(dplyr)
#' 
#' fortaleza <- read_gtfs(system.file("extdata/fortaleza.zip", package = "gtfs2gps"))
#' srtmfile <- system.file("extdata/fortaleza-srtm.tif", package="gtfs2gps")
#'
#' subset <- fortaleza %>%
#'   gtfstools::filter_by_weekday(c("monday", "wednesday")) %>%
#'   filter_single_trip() %>%
#'   gtfstools::filter_by_shape_id("shape806-I")
#' 
#' for_gps <- gtfs2gps(subset)
#' for_gps_sf_points <- gps_as_sfpoints(for_gps)
gps_as_sfpoints <- function(gps, crs = 4326){
  if(is.null(gps$height))
    temp_gps <- sfheaders::sf_point(gps, x = "shape_pt_lon", y = "shape_pt_lat", keep = TRUE)
  else
    temp_gps <- sfheaders::sf_point(gps, x = "shape_pt_lon", y = "shape_pt_lat", z = "height", keep = TRUE)

  dup <- duplicated(names(temp_gps))
  
  if(any(dup))
    temp_gps<- temp_gps[, -dup]
  
  # add projection
  sf::st_crs(temp_gps) <- crs
  return(temp_gps)
}
