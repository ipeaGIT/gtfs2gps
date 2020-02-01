test_that("gtfs2gps", {
    poa <- system.file("extdata/poa.zip", package="gtfs2gps")

    poa_gps <- read_gtfs(poa) %>%
      filter_week_days() %>%
      gtfs2gps(parallel = F)

    #poa_shape <- read_gtfs(poa) %>% gtfs_shapes_as_sf()
    #plot(poa_shape)
    #poa_gps_shape <- gps_as_sf(poa_gps)
    #plot(poa_gps_shape)
    #write_sf(poa_shape, "poa_shape.shp")
    #write_sf(poa_gps_shape, "poa_gps_shape.shp")
    
    my_dim <- dim(poa_gps)[1]
    expect(my_dim %in% c(303851, 303697), paste("length of gtfs incorrect:", my_dim))

    my_length <- length(poa_gps$dist[which(!poa_gps$dist < 15)])
    expect(my_length %in% c(21, 77, 98), paste("incorrect number of distances greater than 15m:", my_length))
    
    expect_equal(sum(poa_gps$dist), 4065814, 0.001)
    
    expect_true(all(names(poa_gps) %in% 
      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat",
        "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))
    
    expect_true(all(!is.na(poa_gps$dist)))

    expect_true(all(poa_gps$dist > 0))
    expect_true(all(poa_gps$speed > 0))
    expect_true(all(poa_gps$cumtime > 0))

    poa_gps_300 <- read_gtfs(poa) %>%
      filter_week_days() %>%
      gtfs2gps(spatial_resolution = 300, parallel = F, progress = FALSE)
    
    expect_equal(dim(poa_gps_300)[1], 66264)
    expect(dim(poa_gps_300)[1] < dim(poa_gps)[1], "more spatial_resolution is not decreasing the number of points")
    
    # save into file
    poa_simple <- read_gtfs(poa) %>%
      filter_by_shape_id(c("T2-1", "A141-1"))

    poa_gps <- gtfs2gps(poa_simple, parallel = F, progress = FALSE, filepath = ".")
    expect_null(poa_gps)
    
    poa_gps <- gtfs2gps(poa, parallel = F, progress = FALSE, filepath = ".", continue = TRUE)
    
    expect_null(poa_gps)
    
    files <- list.files(".", pattern = "\\.txt$")
    names <- gsub('.{4}$', '', files)
    
    poa_shape <- gtfs_shapes_as_sf(read_gtfs(poa))
    expect_setequal(poa_shape$shape_id, names)
    
    file.remove(files)
    
    sp <- system.file("extdata/saopaulo.zip", package="gtfs2gps")

    expect_error(gtfs2gps(sp, parallel = F, continue = TRUE), "Cannot use argument 'continue' without passing a 'filepath'.", fixed = TRUE)

    sp_gps <- read_gtfs(sp) %>%
      filter_by_shape_id(52000:52200) %>%
      filter_week_days() %>%
      filter_single_trip() %>%
      gtfs2gps(parallel = F, progress = TRUE)

    expect_true(all(names(sp_gps) %in% 
      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat",
        "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))

    my_dim <- dim(sp_gps)[1]
    expect(my_dim %in% c(239344, 239362), paste("Wrong dim:", my_dim))
    
    expect_true(all(sp_gps$dist > 0))
    expect_true(all(sp_gps$speed > 0))
    expect_true(all(sp_gps$cumtime > 0))
})
