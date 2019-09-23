
#' @title Test whether a GTFS feed is frequency based
#' @description Test whether a GTFS feed is frequency based or whether the stop_times.txt file
#' presents detailed time table for all routes and trip ids .
#' @param gtfs A GTFS data.
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#' test_gtfs_freq(poa)
#'
test_gtfs_freq <- function(gtfs){
  
# Does the GTFS feed has a frequency.txt file?
  # yes
  if (is.null(gtfs$frequencies) == F) { message("Frequency-based GTFS feed")}
  return("frequency")
  # no
  if (is.null(gtfs$frequencies) == T) { message("Simple GTFS feed")}
  return("simple")
  }
