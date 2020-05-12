#' @title Add a column with height to GPS data
#' @description Add a column named height to GPS data using a tif data as reference.
#' @param gps A GPS data created from gtfs2gps().
#' @param heightfile The pathname of a tif file with height data.
#' @return The GPS data with a new column named height.
#' @export
#' @examples
#' library(dplyr)
#' 
#' fortaleza <- system.file("extdata/fortaleza.zip", package = "gtfs2gps")
#' srtmfile <- system.file("extdata/fortaleza-srtm.tif", package = "gtfs2gps")
#' 
#' gtfs <- read_gtfs(fortaleza) %>%
#'   filter_week_days() %>%
#'   filter_single_trip() %>%
#'   remove_invalid()
#' 
#' fortaleza_gps <- gtfs2gps(gtfs, progress = FALSE) %>% append_height(srtmfile)
append_height <- function(gps, heightfile){
  f_gps <- gps

  sp::coordinates(f_gps) <- ~shape_pt_lon + shape_pt_lat
  
  myraster <- raster::raster(heightfile)
  gps$height <- raster::extract(myraster, f_gps)

  return(gps)
}
