test_that("adjust_arrival_departure", {
    poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

    poa$stop_times[trip_id == "T2-1@1#520" & stop_id == 3608, 
                   arrival_time := data.table::as.ITime("05:21:00", format = "%H:%M:%OS")]
    poa$stop_times[trip_id == "T2-1@1#520" & stop_id == 3564, 
                   departure_time := data.table::as.ITime("05:22:00", format = "%H:%M:%OS")]
    
    poa <- adjust_arrival_departure(poa)

    poa$stop_times[, departure_time := as.numeric(departure_time)]
    poa$stop_times[, arrival_time := as.numeric(arrival_time)]
    
    poa$stop_times <- poa$stop_times[poa$stop_times[, !is.na(departure_time)], ]        
    
    expect_true(all(poa$stop_times$departure_time >= poa$stop_times$arrival_time + 20))
})
