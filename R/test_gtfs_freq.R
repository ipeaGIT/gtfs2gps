
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
  
# Does the GTFS feed has a frequencies.txt file?
  # Yes
    if (is.null(gtfs$frequencies) == F) {
      message("Frequency-based GTFS feed")
      return("frequency")}
    
    
  # No (it might have an empty frequencies.txt file)
    if (is.null(gtfs$frequencies) == T |   is.null(dim(gtfs$frequencies)[1])) { 
      message("Simple GTFS feed")
      return("simple")}

  }

