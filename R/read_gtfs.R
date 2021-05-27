#' @title Read GTFS data into a list of data.tables
#' @description Read files of a zipped GTFS feed and load them to memory as a list of data.tables.
#' It will load the following files: "shapes.txt", "stop_times.txt", "stops.txt", "trips.txt",
#' "agency.txt", "calendar.txt", "routes.txt", and "frequencies.txt", with
#' this last four being optional. If one of the mandatory files does not exit,
#' this function will stop with an error message.
#' @param gtfszip A zipped GTFS data.
#' @param quiet A logical. Whether to hide log messages and progress bars. 
#'              Defaults to `FALSE`.
#' @return A list of data.tables, where each index represents the respective GTFS file name.
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
#' 
read_gtfs <- function(gtfszip, quiet = FALSE){
  # read GTFS feed
  result <- 
    gtfsio::import_gtfs(
      path = gtfszip,
      quiet = quiet,
      # files = c("agency", "routes", "stops", "stop_times", "shapes", "trips", "calendar", "calendar_dates", "frequencies"),
      fields = list(
        routes = c("agency_id", "route_id", "route_type"),
        #stops = c("stop_id", "stop_lat", "stop_lon"),
        stop_times = c("trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence"),
        shapes = c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"),
        trips = c("route_id", "service_id", "trip_id", "shape_id")
      )
    )

  # check columns
  if(is.null(result$shapes)     || dim(result$shapes)[1] == 0)     stop("shapes.txt is empty in the GTFS file")
  if(is.null(result$trips)      || dim(result$trips)[1] == 0)      stop("trips.txt is empty in the GTFS file")
  if(is.null(result$stops)      || dim(result$stops)[1] == 0)      stop("stops.txt is empty in the GTFS file")
  if(is.null(result$stop_times) || dim(result$stop_times)[1] == 0) stop("stop_times.txt is empty in the GTFS file")
  
  if(!is.null(result$frequencies) && dim(result$frequencies)[1] == 0) stop("frequencies.txt is empty in the GTFS file")
  
  mysub <- function(value) sub("^24:", "00:", value)
  
  result$stop_times[, departure_time := data.table::as.ITime(mysub(departure_time), format = "%H:%M:%OS")]
  result$stop_times[, arrival_time := data.table::as.ITime(mysub(arrival_time), format ="%H:%M:%OS")]
  
  if(!is.null(result$frequencies)){
    result$frequencies[, start_time := data.table::as.ITime(mysub(start_time), format = "%H:%M:%OS")]
    result$frequencies[, end_time := data.table::as.ITime(mysub(end_time), format = "%H:%M:%OS")]
  }
  
  return(result)
}