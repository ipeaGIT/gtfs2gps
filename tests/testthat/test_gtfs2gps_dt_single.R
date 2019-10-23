context("gtfs2gps_dt_single")

test_that("gtfs2gps_dt_single", {
    poa <- system.file("extdata/poa.zip", package="gtfs2gps")

    poa_gps <- gtfs2gps_dt_single(poa, progress = FALSE)
    
    #poa_shape <- read_gtfs(poa) %>% gtfs_shapes_as_sf()
    #plot(poa_shape)
    #poa_gps_shape <- gps_as_sf(poa_gps)
    #plot(poa_gps_shape)
    #write_sf(poa_shape, "poa_shape.shp")
    #write_sf(poa_gps_shape, "poa_gps_shape.shp")
    
    expect_equal(dim(poa_gps)[1], 309283)
    
    expect_equal(length(poa_gps$dist[which(!poa_gps$dist < 15)]), 22)

    expect_equal(sum(poa_gps$dist), 4139392, 0.001)
    
    expect_true(all(names(poa_gps) %in% 
      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat",
        "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))
    
    expect_true(all(!is.na(poa_gps$dist)))
})
