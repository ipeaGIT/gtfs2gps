#' @title Adjust the speeds of a gps-like table created with \code{\link{gtfs2gps}} 
#'
#' @description Some GTFS.zip data sets might have quality issues, for example 
#' by assuming that a trip speed is unreasonably high (e.g. an urban bus running
#' over 100 Km/h), or in other cases the `departure_time` information might be
#' missing for some route segments. This can lead a gps-like table to have `NA`
#' or unrealistic `speed` and `departure_time` values. This function allows the
#' user to adjust the speed of trips and updates `departure_time` values 
#' accordingly. The user can adjust the problematic speeds by either setting a
#' custom constant value, or by considering the average of all valid trips speed
#' (Default). The columns `departure_time` and `cumtime` are updated accordingly.
#'
#' @param gps_data A GPS-like data.table created with \code{\link{gtfs2gps}}.
#' @param min_speed Minimum speed (in km/h) to be considered as valid. Values 
#' below minimum speed will be adjusted. Defaults to 2 km/h.
#' @param max_speed Maximum speed (in km/h) to be considered as valid. Values
#' above maximum speed will be adjusted. Defaults to 80 km/h.

#' @param new_speed Speed (in km/h) to replace missing values as well as values
#' outside min_speed and max_speed range. By default, `new_speed = NULL` and the
#' function considers the average speed of the entire gps data.
#' 
#' @return A GPS-like data with adjusted `speed` values. The columns
#' `departure_time` and `cumtime` are also updated accordingly.
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#'
#' poa_gps <- gtfs2gps(poa)
#' poa_gps_new <- adjust_speed(poa_gps)
adjust_speed <- function(gps_data, min_speed = 2, max_speed = 80, new_speed = NULL){
  max_speed <- units::set_units(max_speed, "km/h") %>% 
    units::set_units("m/s") %>%
    as.numeric()
  
  min_speed <- units::set_units(min_speed, "km/h") %>%
    units::set_units("m/s") %>%
    as.numeric()

  gps_data[, speed := units::drop_units(units::set_units(speed, "m/s"))]
  gps_data[speed == "Inf" | is.na(speed) | is.nan(speed), speed := NA]
  gps_data[!data.table::between(x = speed, lower = min_speed, upper = max_speed), speed := NA]
  
  if(is.null(new_speed))
    new_speed <- mean(gps_data$speed, na.rm = TRUE)
  else
    new_speed <- units::set_units(new_speed, "km/h") %>%
      units::set_units("m/s") %>%
      as.numeric()

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
