
#' @title Test whether a GTFS feed is frequency based
#' @description Test whether a GTFS feed is frequency based or whether frequencies.txt
#' presents detailed time table for all routes and trip ids.
#' @param gtfs A GTFS data set stored in memory as a list of data.tables/data.frames.
#' @return A string "frequency" or "simple".
#' @export
#' @examples
#' library(gtfs2gps)
#' 
#' # read a gtfs.zip to memory
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#' 
#' # Test whether a GTFS feed is frequency based
#' test_gtfs_freq(poa)
test_gtfs_freq <- function(gtfs){
  # Does the GTFS feed has a frequencies.txt file?
  
  # No (it might have an empty frequencies.txt file)
  if (is.null(gtfs$frequencies) == T | is.null(dim(gtfs$frequencies)[1])) { 
    return("simple")
  }
  else if (is.null(gtfs$frequencies) == F) {
    return("frequency")
  }
}
