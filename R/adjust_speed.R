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
  gps_data <- data.table::copy(gps_data)

  max_speed <- units::set_units(max_speed, "km/h") %>% 
    units::set_units("m/s") %>%
    units::drop_units()
  
  min_speed <- units::set_units(min_speed, "km/h") %>%
    units::set_units("m/s") %>%
    units::drop_units()

  gps_data[, speed := units::drop_units(units::set_units(speed, "m/s"))]
  gps_data[speed == "Inf" | is.na(speed) | is.nan(speed), speed := NA]
  gps_data[!data.table::between(x = speed, lower = min_speed, upper = max_speed), speed := NA]
  
  if(is.null(new_speed))
    new_speed <- mean(gps_data$speed, na.rm = TRUE)
  else
    new_speed <- units::set_units(new_speed, "km/h") %>%
      units::set_units("m/s") %>%
      units::drop_units()

  gps_data[, dist := units::drop_units(dist)]
  gps_data[, cumdist := units::drop_units(cumdist)]
  gps_data[, cumtime := units::drop_units(cumtime)]
  
  gps_data[is.na(speed), speed := new_speed]

  gps_data[, time := dist / speed]
  gps_data[, cumtime := cumsum(time), by = .(shape_id, trip_id, trip_number)]
  
  gps_data <- lapply(unique(gps_data$shape_id), function(i){
    tmp_gps_data <- gps_data[shape_id == i, ]
    
    if(nrow(tmp_gps_data[!is.na(departure_time)]) < 2){
     message(paste0("departure_time column of shape_id ", i, " was not adjusted as there are less than two departure_time's."))
     return(tmp_gps_data)
    }
    
    id_first <- which(!is.na(tmp_gps_data$departure_time))[1]
    
    if(length(id_first) == 0){
      message(paste0("departure_time column of shape_id ", i, " was not adjusted as there is no valid departure_time's."))
      return(tmp_gps_data)
    }
    else if(id_first == 1){
      tmp_gps_data[, 
        departure_time := round(departure_time[1] + cumtime),
        by = .(shape_id, trip_id, trip_number)]
    }else{
      # before the first valid observation
      tmp_gps_data[1:id_first,
        departure_time := round(departure_time[id_first] - cumtime),
        by = .(shape_id, trip_id, trip_number)]
     
      # after the first valid observation
      tmp_gps_data[id_first:.N,
        departure_time := round(departure_time[id_first] + cumtime),
        by = .(shape_id, trip_id, trip_number)]
    }
    
    # midnight trips fix
    tmp_gps_data[as.numeric(departure_time) > 86400, departure_time := departure_time - 86400]
    
    return(tmp_gps_data)
  }) %>% data.table::rbindlist()

  gps_data[, speed := units::set_units(units::set_units(speed, "m/s"), "km/h")]
  gps_data[, dist := units::set_units(dist, "m")]
  gps_data[, cumdist := units::set_units(cumdist, "m")]
  gps_data[, time := units::set_units(time, "s")]
  gps_data[, cumtime := units::set_units(cumtime, "s")]

  gps_data[, departure_time := data.table::as.ITime(departure_time)]

  return(gps_data)
}
