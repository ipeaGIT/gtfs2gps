context("gtfs2gps")

test_that("gtfs2gps_dt_parallel", {
    poa <- system.file("extdata/poa.zip", package="gtfs2gps")

    poa_gps <- gtfs2gps_dt_parallel(poa, progress = FALSE)
    expect(dim(poa_gps)[1] %in% c(309283, 309129))
    
    expect(length(poa_gps$dist[which(!poa_gps$dist < 15)]) %in% c(22, 77))
    
    expect_equal(sum(poa_gps$dist), 4139392, 0.001)
    
    expect_true(all(names(poa_gps) %in% 
      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat",
        "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed")))
    
    expect_true(all(!is.na(poa_gps$dist)))
})
