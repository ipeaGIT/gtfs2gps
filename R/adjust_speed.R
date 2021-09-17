
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
#' @param new_speed Speed to replace missing values as well as values outside
#' min_speed and max_speed range. The default value is the average speed of the
#' gps data.
#' @details This function estimates speed values using the average speed of the
#' whole GPS data, ignoring infinite, NA, and NAN values. Values above 80km/h and
#' below 2km/h are also ignored and their values will be updated with the average.
#' @return A GPS data with updated values of cumtime.
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#'
#' poa_gps <- gtfs2gps(poa)
#' poa_gps_new <- adjust_speed(poa_gps)
adjust_speed <- function(gps_data, min_speed = 2, max_speed = 80, new_speed = NULL){
#  gps_data <- poa_gps
#  min_speed = 2
#  max_speed = 80
  
  max_speed <- units::set_units(max_speed, "km/h") %>% 
    units::set_units("m/s") %>%
    as.numeric()
  
  min_speed <- units::set_units(min_speed, "km/h") %>%
    units::set_units("m/s") %>%
    as.numeric()

  gps_data[, speed := units::drop_units(units::set_units(speed, "m/s"))]
  gps_data[speed == "Inf" | is.na(speed) | is.nan(speed), speed := NA]
  gps_data[!data.table::between(x = speed, lower = min_speed, upper = max_speed), speed := NA]
  
  if(is.null(new_speed)) new_speed <- mean(gps_data$speed, na.rm = TRUE)

  gps_data[is.na(speed), speed := new_speed]
  gps_data[, speed := units::set_units(speed, "m/s")]
  gps_data[, speed := units::set_units(speed, "km/h")]

  gps_data[, time := units::set_units(dist / speed, "s")]
  gps_data[, cumtime := cumsum(time), by = .(shape_id, trip_id, trip_number)]

  gps_data[, departure_time := as.numeric(departure_time)]
  gps_data[, departure_time := units::set_units(departure_time, "s")]
  gps_data[, departure_time := data.table::first(departure_time) + cumtime, by = .(shape_id, trip_id, trip_number)]
  
  gps_data[, departure_time := units::drop_units(departure_time)]
  gps_data[, departure_time := data.table::as.ITime(departure_time)]
  gps_data[, time := NULL]
  
  return(gps_data)
}
