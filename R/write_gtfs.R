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
  tempd <- file.path(tempdir(), "gtfsdir") # create temp dir to save GTFS unzipped files
  unlink(normalizePath(paste0(tempd, "/", dir(tempd)), mustWork = FALSE), recursive = TRUE) # clean tempfiles in that dir

  for(attr in names(gtfs))
    data.table::fwrite(gtfs[[attr]], paste0(tempd, "/", attr, ".txt"))

  # utils::zip(zipfile = zipfile, files = list.files(tempd, full.names = TRUE), flags = "-jr9Xq")
  zip::zipr(zipfile = zipfile, files = list.files(tempd, full.names = TRUE))
}
