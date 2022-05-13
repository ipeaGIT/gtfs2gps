#' @title Adjust the arrival and departure times of a GTFS data
#'
#' @description Some GTFS.zip data have issues related to arrival and departure
#' time on stops. This function makes sure the GTFS has dis/embarking times at
#' each stop. For each stop time row, this function applies the following steps:
#' 
#' 1. If there is `arrival_time` but no `departure_time`, it creates a departure_time 
#' column by summing the arrival plus a pre-defined `min_lag`.
#' 
#' 2. If there is `departure_time` but no `arrival_time`, it creates an arrival_time 
#' column by subtracting a pre-defined `min_lag` from the departure.
#' 
#' 3. If there is an `arrival_time` and a `departure_time` but their difference
#' is smaller than `min_lag`, it reduces the `arrival_time` and increases
#' `departure_time` so that the difference will be exactly `min_lag`.
#'
#' @param gtfs_data A GTFS data created with \code{\link{read_gtfs}}.
#' @param min_lag Numeric. Minimum waiting time when a vehicle arrives 
#' at a stop. It can be a numeric or a units value that can be converted
#' to seconds. Default is 20s.
#' @return A GTFS with adjusted `arrival_time` and `departure_time` on
#' data.table `stop_times`.
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#'
#' poa <- adjust_arrival_departure(poa)
adjust_arrival_departure <- function(gtfs_data, min_lag = 20){
  if(is.null(gtfs_data$stop_times$arrival_time))
    gtfs_data$stop_times[, arrival_time := NA]
  
  if(is.null(gtfs_data$stop_times$departure_time))
    gtfs_data$stop_times[, departure_time := NA]
  
  min_lag <- as.numeric(units::set_units(min_lag, "s"))
  
  gtfs_data$stop_times[, departure_time := gtfstools:::string_to_seconds(departure_time)]
  gtfs_data$stop_times[, arrival_time := gtfstools:::string_to_seconds(arrival_time)]

  gtfs_data$stop_times[is.na(arrival_time) & !is.na(departure_time), 
                       arrival_time := departure_time - min_lag]

  gtfs_data$stop_times[is.na(departure_time) & !is.na(arrival_time), 
                       departure_time := arrival_time + min_lag]

  gtfs_data$stop_times[!is.na(departure_time) & !is.na(arrival_time) & departure_time - arrival_time < min_lag, 
                       diff := (min_lag - departure_time + arrival_time) / 2]
  
  gtfs_data$stop_times[!is.na(diff), arrival_time := arrival_time - diff]
  gtfs_data$stop_times[!is.na(diff), departure_time := departure_time + diff]
  
  gtfs_data$stop_times[, diff := NULL]

  gtfs_data$stop_times[, departure_time := gtfstools:::seconds_to_string(departure_time)]
  gtfs_data$stop_times[, arrival_time := gtfstools:::seconds_to_string(arrival_time)]
  
  return(gtfs_data)
}
