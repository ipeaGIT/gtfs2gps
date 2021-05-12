#' @title Write GTFS data into a zip file
#' @description Write GTFS stored in memory as a list of data.tables into a zipped GTFS feed.
#' This function overwrites the zip file if it exists.
#' @param gtfs A GTFS data set stored in memory as a list of data.tables/data.frames.
#' @param zipfile The pathname of a .zip file to be saved with the GTFS data.
#' @param overwrite A logical. Whether to overwrite an existing \code{.zip} file.
#'        Defaults to \code{TRUE}.
#' @param quiet A logical. Whether to hide log messages and progress bars. 
#'        Defaults to \code{TRUE}.
#'        
#' @return The status value returned by the external zip command, invisibly.
#' @export
#' @examples
#' library(dplyr)
#' 
#' # read a gtfs.zip to memory
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps")) %>%
#'   filter_by_shape_id("T2-1") %>%
#'   filter_single_trip()
#' 
#' # write GTFS data into a zip file
#' write_gtfs(poa, paste0(tempdir(), "/mypoa.zip"))
#' 
write_gtfs <- function(gtfs, zipfile, overwrite = TRUE, quiet = FALSE){
  
  gtfsio::export_gtfs(gtfs = gtfs, 
                      path = zipfile, 
                      overwrite = TRUE, 
                      quiet = quiet
  )
}