update_freq <- function(tripid, new_stoptimes, gtfs_data, all_tripids){
  # Update new_stoptimes
  new_stoptimes <- update_dt(tripid, new_stoptimes, gtfs_data, all_tripids)

  if(is.null(new_stoptimes)){
    return(new_stoptimes) # nocov
  }

  new_stoptimes[, "trip_number"] <- 1
  
  if(is.null(gtfs_data$frequencies)) return(new_stoptimes) # nocov
  
  #  Get freq info for that trip
  # tripid <- "8700-21-0"
  freq_temp <- subset(gtfs_data$frequencies, trip_id == tripid)
  
  if(dim(freq_temp)[1] == 0) return(new_stoptimes)
  
  # number of trips
  freq_temp[, service_duration := abs(end_time[1] - start_time[1])]
  freq_temp[, number_of_departures := ceiling(service_duration / headway_secs)]
  
  # get all start times of each period
  starttimes <- freq_temp$start_time #- new_stoptimes$cumtime[newstop_t0[1]-1]
  
  # functions
  update_newstoptimes <- function(starttimes, freq_temp){
    update_departure_stoptimes <- function(i, dt_list){
      # Update 1st departure time
      dt_list[[i]][ departure_time == data.table::first(departure_time),
                    departure_time := starttimes[1]]
      # Updating all other stop times according to travel speed and distances
      dt_list[[i]][, departure_time := round(departure_time[1L] + stats::lag(cumtime, 1, 0))]
      # dt_list[[i]][, departure_time := departure_time[1L] +
      #                                                         stats::lag(cumtime,1,0)]
      
      # Updating all stop times by adding the headway
      dt_list[[i]][, departure_time := round(departure_time + ((i - 1) * thisheadway))]
      dt_list[[i]][, trip_number := i]
      return(dt_list[[i]])
    }
    
    #starttimes <- starttimes[1]
    
    # Get headway of each start_time
    thisheadway <- subset(freq_temp, start_time == starttimes[1])$headway_secs
    nmber_of_departures <- subset(freq_temp, start_time == starttimes[1])$number_of_departures

    if(length(nmber_of_departures) == 0 || is.na(nmber_of_departures)){
      warning(paste0("Trip '", tripid, "' has zero departures. Ignoring it."),  call. = FALSE) # nocov
      return(NULL) # nocov
    }

#    if(nmber_of_departures < 0) nmber_of_departures <- -nmber_of_departures
      
    # list of departures
    departure_list <- 1:nmber_of_departures
    
    # # Replicate one new_stop_times for each departure  
    # all_departures <- rep(list(new_stoptimes), nmber_of_departures)
    dt_list <- replicate(nmber_of_departures, list(data.table::copy(new_stoptimes)))
    
    # Function to update stoptimes of each departure
    dt_list <- lapply(departure_list, update_departure_stoptimes, dt_list)
    
    # Apply function and return the stop times of all departures from that period
    departure_stoptimes <- lapply(X = seq_along(dt_list), FUN = update_departure_stoptimes, dt_list) %>%
      data.table::rbindlist()
    
    #departure_stoptimes <- lapply(X = departure_list, FUN = update_departure_stoptimes) %>% data.table::rbindlist()
    return(departure_stoptimes)
  }
  
  new_stoptimes <- lapply(starttimes, update_newstoptimes, freq_temp) %>%
    data.table::rbindlist()
  
  #departure_stoptimes <- update_newstoptimes_freq(starttime)
  return(new_stoptimes)
}

# UPDATE NEWSTOPTIMES DATA.FRAME
update_dt <- function(tripid, new_stoptimes, gtfs_data, all_tripids){
                    # internal test
                    # tripid <- "176-1@1#1800" all_tripids[1]
  # message(tripid)
  # add trip_id 
  new_stoptimes[, trip_id := tripid]
  
  # add cummulative distance
  new_stoptimes[, cumdist := cumsum(dist)]

  # subset original stoptimes to get original travel_times btwn stops
  stoptimes_temp <- gtfs_data$stop_times[trip_id == tripid]
  
  # add departure_time based on stop sequence
  new_stoptimes[stoptimes_temp, on = 'stop_sequence', 'departure_time' := i.departure_time]

  # get a 'stop_sequence' of the stops which have proper info on 'departure_time'
  stop_id_ok <- gtfs_data$stop_times[trip_id == tripid & is.na(departure_time) == FALSE,]$stop_sequence
  
  # ignore trip_id if original departure_time values are missing
  if(is.null(length(stop_id_ok)) == TRUE | length(stop_id_ok) == 1 | length(stop_id_ok) == 0){ 
    warning(paste0("Trip '", tripid, "' has less than two stop_ids. Ignoring it."),  call. = FALSE) # nocov
    return(NULL) # nocov
  }
    
  ### UPDATE speeds
  # lim0: 'id' in which stop_times intervals STARTS
  lim0 <- new_stoptimes[ !is.na(departure_time) & !is.na(stop_id), id]
  #  function for speed estimation
  update_speeds <- function(i){
    a <- lim0[i]
    b <- lim0[i + 1]
    new_stoptimes[a:b, speed := 3.6 * (data.table::last(cumdist) - data.table::first(cumdist)) / (data.table::last(departure_time) - data.table::first(departure_time)) ]
  }
  
  # apply function for speed estimation
  L <- length(lim0)
  lapply(X = 1:(L-1), FUN = update_speeds)

  # Speed info that was missing (either before or after 1st/last stops)
  # Get trip duration in seconds
  new_stoptimes[, cumtime := cumsum(3.6 * dist / speed)]
  
  # reorder columns
  data.table::setcolorder(new_stoptimes, c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "departure_time", "stop_id", "stop_sequence", "dist", "cumdist", "speed", "cumtime"))
  # distance from trip start to 1st stop
  dist_1st <- new_stoptimes[id == lim0[1]]$cumdist # in m
  # get the depart time from 1st stop
  departtime_1st <- as.numeric(new_stoptimes[id == lim0[1]]$departure_time)
  departtime_1st <- departtime_1st - (3.6 * dist_1st / new_stoptimes$speed[1]) # time in seconds
  
  # Determine the start time of the trip (time stamp the 1st GPS point of the trip)
  suppressWarnings(new_stoptimes[id == 1, departure_time := round(departtime_1st)])
  
  # recalculate time stamps, except the given 'departure_time's from stop sequences
  #stop_id_nok <- which(is.na(new_stoptimes$departure_time))
  # update indexes in 'newstoptimes'
  #temp_newdeparture <- new_stoptimes$departure_time[1L]+stats::lag(new_stoptimes$cumtime,1,0)
  new_stoptimes[, departure_time := round(departure_time[1L] + stats::lag(cumtime, 1, 0))]
  

  if(is.null(new_stoptimes)){
    warning(paste0("Could not create stop times for trip '", tripid, "'. Ignoring it."),  call. = FALSE) # nocov
  }
  else if(dim(new_stoptimes)[1] == 0)
    warning(paste0("Trip '", tripid, "' has zero GPS points. Ignoring it."),  call. = FALSE) # nocov
  
  return(new_stoptimes)
}
