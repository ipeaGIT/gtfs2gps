update_freq <- function(tripid, new_stoptimes, gtfs_data, all_tripids){
  # Update new_stoptimes
  new_stoptimes <- update_dt(tripid, data.table::copy(new_stoptimes)
                              , gtfs_data
                              , all_tripids)
  if(is.null(new_stoptimes)){
    return(new_stoptimes) # nocov
  }
  
  if(is.null(gtfs_data$frequencies)) {
    new_stoptimes[, trip_number := tripid ]
    return(new_stoptimes) # nocov
  }  
  #  Get freq info for that trip
  # tripid <- "8700-21-0"
  freq_temp <- subset(gtfs_data$frequencies, trip_id == all_tripids[tripid])
  
  if(dim(freq_temp)[1] == 0) return(new_stoptimes)
  
  # number of trips
  freq_temp[, service_duration := abs(end_time[1] - start_time[1])]
  freq_temp[, number_of_departures := ceiling(service_duration / headway_secs)]
  # update number of trips
  freq_temp[, start_trip_number := data.table::shift(cumsum(number_of_departures),1,0) +1]
  freq_temp[, end_trip_number := start_trip_number + number_of_departures - 1]
  # get all start times of each period
  starttimes <- freq_temp$start_time #- new_stoptimes$cumtime[newstop_t0[1]-1]
  
  # functions
  update_newstoptimes <- function(starttimes, freq_temp){
    update_departure_stoptimes <- function(i, dt_list){
      # Update 1st departure time
      dt_list[[i]][ departure_time == data.table::first(departure_time),
                    departure_time := starttimes[1]]
      # Updating all other stop times according to travel speed and distances
      dt_list[[i]][, departure_time := departure_time[1L] + cumtime + cumsum(lag)]
      dt_list[[i]][, arrival_time := departure_time - lag]
      # dt_list[[i]][, departure_time := departure_time[1L] +
      #                                                         stats::lag(cumtime,1,0)]
      
      # Updating all stop times by adding the headway
      dt_list[[i]][, departure_time := round(departure_time + ((i - 1) * thisheadway))]
      dt_list[[i]][, arrival_time := round(arrival_time + ((i - 1) * thisheadway))]
      dt_list[[i]][, trip_number := departure_list[i]]
      return(dt_list[[i]])
    }
    
    #starttimes <- starttimes[1]
    
    # Get headway of each start_time
    thisheadway <- subset(freq_temp, start_time == starttimes[1])$headway_secs
    nmber_of_departures <- subset(freq_temp, start_time == starttimes[1])$number_of_departures
    
    if(length(nmber_of_departures) == 0 || is.na(nmber_of_departures)){
      message(paste0("Trip '", tripid, "' has zero departures. Ignoring it.")) # nocov
      return(NULL) # nocov
    }
    
    #    if(nmber_of_departures < 0) nmber_of_departures <- -nmber_of_departures
    
    # # list of departures
    # departure_list <- 1:nmber_of_departures
    # list of departures
    departure_list <- subset(freq_temp, start_time == starttimes[1])[,c(start_trip_number,end_trip_number)]
    departure_list <- departure_list[1]:departure_list[2]
    
    # # Replicate one new_stop_times for each departure  
    # all_departures <- rep(list(new_stoptimes), nmber_of_departures)
    dt_list <- replicate(nmber_of_departures, list(data.table::copy(new_stoptimes)))
    
    # Function to update stoptimes of each departure
    dt_list <- lapply(seq_along(departure_list), update_departure_stoptimes, dt_list)
    
    # Apply function and return the stop times of all departures from that period
    departure_stoptimes <- lapply(X = seq_along(dt_list), FUN = update_departure_stoptimes, dt_list)
    departure_stoptimes <- data.table::rbindlist(departure_stoptimes)
    departure_stoptimes[,trip_id := paste0(trip_id,"#",trip_number)]
    #departure_stoptimes <- lapply(X = departure_list, FUN = update_departure_stoptimes) %>% data.table::rbindlist()
    return(departure_stoptimes)
  }
  
  new_stoptimes <- lapply(starttimes, update_newstoptimes, freq_temp)
  new_stoptimes <- data.table::rbindlist(new_stoptimes)
  
  #departure_stoptimes <- update_newstoptimes_freq(starttime)
  return(new_stoptimes)
}

# UPDATE NEWSTOPTIMES DATA.FRAME
update_dt <- function(tripid, new_stoptimes, gtfs_data, all_tripids){
  # internal test
  # tripid <- "176-1@1#1800" all_tripids[1]
  # add trip_id 
  new_stoptimes[, trip_id := all_tripids[tripid]]
  
  # add cummulative distance
  new_stoptimes[, cumdist := cumsum(dist)]
  
  # subset original stoptimes to get original travel_times btwn stops
  stoptimes_temp <- gtfs_data$stop_times[trip_id == all_tripids[tripid]]
  
  # add departure_time based on stop sequence
  new_stoptimes[stoptimes_temp, on = 'stop_sequence', `:=`(
    'departure_time' = i.departure_time,
    'arrival_time' = i.arrival_time)]
  
  # get a 'stop_sequence' of the stops which have proper info on 'departure_time'
  stop_id_ok <- gtfs_data$stop_times[trip_id == all_tripids[tripid] & 
                                       is.na(departure_time) == FALSE,]$stop_sequence
  
  # ignore trip_id if original departure_time values are missing
  if(is.null(length(stop_id_ok)) == TRUE | length(stop_id_ok) == 1 | length(stop_id_ok) == 0){ 
    message(paste0("Trip '", all_tripids[tripid], "' has less than two stop_ids. Ignoring it.")) # nocov
    return(NULL) # nocov
  }
  
  new_stoptimes[, speed := numeric()]
  
  # lim0: 'id' in which stop_times intervals STARTS
  lim0 <- new_stoptimes[ !is.na(departure_time) & !is.na(stop_id), id]
  
  new_points <- data.table::copy(new_stoptimes[lim0, ])
  new_points[, departure_time := arrival_time]
  new_points[, id := id - 0.1]

  new_stoptimes[lim0, dist := 0]

  new_stoptimes <- rbind(new_stoptimes, new_points)
  data.table::setorder(new_stoptimes, "id")
  new_stoptimes$id <- 1:dim(new_stoptimes)[1]

  new_stoptimes[, timestamp := data.table::as.ITime(departure_time)]

  new_stoptimes[1, speed := 1e-12]
  new_stoptimes[lim0 + 1, speed := 1e-12]
  new_stoptimes[, cumtime := 0]

  lim0 <- new_stoptimes[ !is.na(timestamp) & !is.na(stop_id), id]
  #  function for speed estimation
  update_speeds <- function(i){
    a <- lim0[i]
    b <- lim0[i + 1]

    diff_timestamp <- new_stoptimes$timestamp[b] - new_stoptimes$timestamp[a]
    if(diff_timestamp < 0) diff_timestamp <- diff_timestamp + 86400 # one day in seconds
    
    if(a + 1 == b) {
      value <- new_stoptimes[a, cumtime] + diff_timestamp
      new_stoptimes[b, cumtime := value]
      new_stoptimes[b, speed := 1e-12]
      return() # two consecutive points with arrival_time don't need to be interpolated
    }
    
    increment <- diff_timestamp / (b - a)
    
    new_stoptimes[a:b, cumtime := .I * increment + data.table::first(cumtime)]
    new_stoptimes[a:b, speed := 3.6 * dist / (cumtime - data.table::shift(cumtime))]
    # cumtime is related to row a, therefore it cannot be (a+1):b
    
    new_stoptimes[a, speed := 1e-12]
    
    new_stoptimes[a:b, timestamp := data.table::first(timestamp) + round(cumtime - data.table::first(cumtime))]
  }
  
  lapply(1:(length(lim0) - 1), FUN = update_speeds)

  # Get lag
  #new_stoptimes[!is.na(departure_time) & !is.na(stop_id)
  #              ,lag := departure_time - arrival_time]
  #new_stoptimes[is.na(lag), lag := 0]
  # Speed info that was missing (either before or after 1st/last stops)
  # Get trip duration in seconds
#  new_stoptimes[, cumtime := cumsum(3.6 * dist / speed)]
  
  # reorder columns
  data.table::setcolorder(new_stoptimes, c("trip_id", "route_type", "id", 
                                           "shape_pt_lon", "shape_pt_lat", 
                                           "departure_time", "stop_id", 
                                           "stop_sequence", "dist", "cumdist",
                                           "speed", "cumtime"))
  
  # distance from trip start to 1st stop
#  dist_1st <- new_stoptimes[id == lim0[1]]$cumdist # in m
  
  # get the depart/arrival time from 1st stop
  #departtime_1st <- as.numeric(new_stoptimes[id == lim0[1]]$departure_time)
  #departtime_1st <- departtime_1st - (3.6 * dist_1st / new_stoptimes$speed[1]) # time in seconds
#  arrival_1st <- as.numeric(new_stoptimes[id == lim0[1]]$arrival_time)
#  arrival_1st <- arrival_1st - (3.6 * dist_1st / new_stoptimes$speed[1]) # time in seconds
  
  
  # Determine the start time of the trip (time stamp the 1st GPS point of the trip)
  #suppressWarnings(new_stoptimes[id == 1, departure_time := round(departtime_1st)])
#  suppressWarnings(new_stoptimes[id == lim0[1], arrival_time := round(arrival_1st)]) 
  
  # recalculate time stamps, except the given 'departure_time's from stop sequences
  #stop_id_nok <- which(is.na(new_stoptimes$departure_time))
  # update indexes in 'newstoptimes'
 # new_stoptimes[, departure_time := departure_time[lim0[1]] +  cumtime + cumsum(lag)]
#  new_stoptimes[, arrival_time := departure_time - lag]
  
  # round
#  new_stoptimes[, timestamp := round(timestamp)]
#  new_stoptimes[, arrival_time := round(arrival_time)]
  
  if(is.null(new_stoptimes)){
    message(paste0("Could not create stop times for trip '", 
                   all_tripids[tripid], "'. Ignoring it.")) # nocov
  }
  else if(dim(new_stoptimes)[1] == 0)
    message(paste0("Trip '", all_tripids[tripid], 
                   "' has zero GPS points. Ignoring it.")) # nocov
  
  return(new_stoptimes)
}
