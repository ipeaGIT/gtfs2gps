#' @title Write GTFS data into a zip file
#' @description Write GTFS stored in memory as a list of data.tables into a zipped GTFS feed.
#' This function overwrites the zip file if it exists.
#' @param gtfs A GTFS data set stored in memory as a list of data.tables/data.frames.
#' @param zipfile The pathname of a .zip file to be saved with the GTFS data.
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
write_gtfs <- function(gtfs, zipfile){
  tempd <- file.path(tempdir(), "gtfsdir") # create tempr dir to save GTFS unzipped files
  unlink(normalizePath(paste0(tempd, "/", dir(tempd)), mustWork = FALSE), recursive = TRUE) # clean tempfiles in that dir

  if(!is.null(gtfs$agency))      data.table::fwrite(gtfs$agency,      paste0(tempd, "/agency.txt"))
  if(!is.null(gtfs$routes))      data.table::fwrite(gtfs$routes,      paste0(tempd, "/routes.txt"))
  if(!is.null(gtfs$stops))       data.table::fwrite(gtfs$stops,       paste0(tempd, "/stops.txt"))
  if(!is.null(gtfs$stop_times))  data.table::fwrite(gtfs$stop_times,  paste0(tempd, "/stop_times.txt"))
  if(!is.null(gtfs$shapes))      data.table::fwrite(gtfs$shapes,      paste0(tempd, "/shapes.txt"))
  if(!is.null(gtfs$trips))       data.table::fwrite(gtfs$trips,       paste0(tempd, "/trips.txt"))
  if(!is.null(gtfs$calendar))    data.table::fwrite(gtfs$calendar,    paste0(tempd, "/calendar.txt"))
  if(!is.null(gtfs$frequencies)) data.table::fwrite(gtfs$frequencies, paste0(tempd, "/frequencies.txt"))

  utils::zip(zipfile = zipfile, files = list.files(tempd, full.names = TRUE), flags = "-jr9Xq")
}
