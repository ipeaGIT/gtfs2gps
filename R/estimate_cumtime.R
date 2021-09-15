
#' @title Estimate cumtime based on global speed
#'
#' @description Estimate cumtime of NA values of a GPS data based on the average
#' speed of the valid GPS rows.
#'
#' @param gps_data A data.table with GPS-like rows.
#' @param min_speed Minimum speed to be considered as valid. Values below minimum
#' speed will be estimated as if they were NA. Default value is 2km/h.
#' @param max_speed Maximum speed to be considered as valid. Values above maximum
#' speed will be estimated as if they were NA. Default value is 80km/h.
#' @details This function estimates speed values using the average speed of the
#' whole GPS data, ignoring infinite, NA, and NAN values. Values above 80km/h and
#' below 2km/h are also ignored and their values will be updated with the average.
#' @return A GPS data with updated values of cumtime.
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#'
#' poa_gps <- gtfs2gps(poa)
#' poa_gps_new <- estimate_cumtime(poa_gps)
estimate_cumtime <- function(gps_data, min_speed = 2, max_speed = 80){
  gps_data[, speed := as.numeric(speed)]
  gps_data[speed == "Inf" | is.na(speed) | is.nan(speed), speed := NA]
  gps_data[speed > max_speed | speed < min_speed, speed := NA] # too slow or too fast

  gps_data[is.na(speed), speed := mean(gps_data$speed, na.rm = TRUE), by = .(shape_id)]
  gps_data[, speed := units::set_units(speed, "km/h")]

  gps_data[, time := (dist / speed)]
  gps_data[, cumtime := cumsum(time), by = .(shape_id, trip_id, trip_number)]
  gps_data[, cumtime := units::set_units(cumtime, "s")]
  
  gps_data[, time := NA]

  return(gps_data)
}
