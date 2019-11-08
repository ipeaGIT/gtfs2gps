context("gtfs2gps")

test_that("gtfs2gps", {
    poa <- system.file("extdata/poa.zip", package="gtfs2gps")

    poa_gps <- gtfs2gps(poa, progress = FALSE)
    
    #poa_shape <- read_gtfs(poa) %>% gtfs_shapes_as_sf()
    #plot(poa_shape)
    #poa_gps_shape <- gps_as_sf(poa_gps)
    #plot(poa_gps_shape)
    #write_sf(poa_shape, "poa_shape.shp")
    #write_sf(poa_gps_shape, "poa_gps_shape.shp")
    
    expect(dim(poa_gps)[1] %in% c(303851, 309283, 309129), "length of gtfs incorrect")
    
    expect(length(poa_gps$dist[which(!poa_gps$dist < 15)]) %in% c(21, 22, 77), "incorrect number of distances greater than 15m")
    
    expect_equal(sum(poa_gps$dist), 4066410, 0.001)
    
    expect_true(all(names(poa_gps) %in% 
      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat",
        "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))
    
    expect_true(all(!is.na(poa_gps$dist)))

    expect_true(all(poa_gps$dist > 0))
    expect_true(all(poa_gps$speed > 0))
    expect_true(all(poa_gps$cumtime > 0))
    # save into file
    poa_gps <- gtfs2gps(poa, progress = FALSE, filepath = ".")
    expect_null(poa_gps)
    
    files <- list.files(".", pattern = "\\.txt$")
    names <- gsub('.{4}$', '', files)
    
    poa_shape <- gtfs_shapes_as_sf(read_gtfs(poa))
    expect_setequal(poa_shape$shape_id, names)
    
    file.remove(files)
    
    # run with a larger dataset
    sp <- system.file("extdata/saopaulo.zip", package="gtfs2gps")

    sp_gps <- gtfs2gps(sp, progress = FALSE)

    expect_true(all(names(sp_gps) %in% 
      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat",
        "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))

    expect_equal(dim(sp_gps)[1], 20418074)
    
    expect_true(all(sp_gps$dist > 0))
    expect_true(all(sp_gps$speed > 0))
    expect_true(all(sp_gps$cumtime > 0))
})
