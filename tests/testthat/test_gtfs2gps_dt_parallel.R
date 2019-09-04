context("gtfs2gps")

test_that("gtfs2gps_dt_parallel", {
    poa <- system.file("extdata/poa.zip", package="gtfs2gps")

    poa_gps <- gtfs2gps_dt_parallel(poa)
    
    expect_equal(dim(poa_gps)[1], 309477)

    expect_true(all(names(poa_gps) %in% 
      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat",
        "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist")))
})
