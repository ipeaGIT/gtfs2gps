test_that("gtfs2gps", {
    poa <- system.file("extdata/poa.zip", package="gtfs2gps")

    poa_gps <- read_gtfs(poa) %>%
      filter_week_days() %>%
      gtfs2gps()

    #poa_shape <- read_gtfs(poa) %>% gtfs_shapes_as_sf()
    #plot(poa_shape)
    #poa_gps_shape <- gps_as_sf(poa_gps)
    #plot(poa_gps_shape)
    #write_sf(poa_shape, "poa_shape.shp")
    #write_sf(poa_gps_shape, "poa_gps_shape.shp")
    
    my_dim <- dim(poa_gps)[1]
    expect_equal(my_dim, 128155)

    poa_gps <- poa_gps[speed > units::set_units(0, "km/h") & cumtime > units::set_units(0, "s") & !is.na(speed) & !is.infinite(speed),]

    expect_true(all(na.omit(poa_gps$speed) > units::set_units(0, "km/h")))
    
    my_dim <- dim(poa_gps)[1]
    expect_equal(my_dim, 81037)
    
    my_length <- length(poa_gps$dist[which(!poa_gps$dist < units::set_units(50, "m"))])
    expect_equal(my_length, 0)
    
    expect_equal(sum(poa_gps$dist), 2544820, 0.1)

    expect_true(all(poa_gps$trip_number == 1))
        
    expect_true(all(names(poa_gps) %in% 
      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "trip_number",
        "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))
    
    expect_true(all(!is.na(poa_gps$dist)))
    
    expect_true(all(poa_gps$dist > units::set_units(0, "m")))
    expect_true(all(poa_gps$cumdist > units::set_units(0, "m")))
    expect_true(all(poa_gps$speed > units::set_units(0, "km/h")))
    expect_true(all(poa_gps$cumtime > units::set_units(0, "s")))

    poa_gps_300 <- read_gtfs(poa) %>%
      filter_week_days() %>%
      gtfs2gps(spatial_resolution = 300)
    
    expect_equal(dim(poa_gps_300)[1], 67571)
    expect(dim(poa_gps_300)[1] < dim(poa_gps)[1], "more spatial_resolution is not decreasing the number of points")
    
    # save into file
    poa_simple <- read_gtfs(poa) %>%
      filter_by_shape_id(c("T2-1", "A141-1"))

    poa_gps <- gtfs2gps(poa_simple, filepath = ".")
    expect_null(poa_gps)
    
    poa_gps <- gtfs2gps(poa, filepath = ".", continue = TRUE)

    expect_null(poa_gps)
    
    files <- list.files(".", pattern = "\\.txt$")
    names <- gsub('.{4}$', '', files)
    
    poa_shape <- gtfs_shapes_as_sf(read_gtfs(poa))
    expect_setequal(poa_shape$shape_id, names)
    
    file.remove(files)
    
    sp <- system.file("extdata/saopaulo.zip", package="gtfs2gps")

    expect_error(gtfs2gps(sp, continue = TRUE), "Cannot use argument 'continue' without passing a 'filepath'.", fixed = TRUE)

    sp_gps <- read_gtfs(sp) %>%
      filter_by_shape_id(52000:52200) %>%
      filter_week_days() %>%
      filter_single_trip() %>%
      gtfs2gps(parallel = TRUE, spatial_resolution = 15)

    expect_true(all(names(sp_gps) %in% 
      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "trip_number",
        "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))

    expect_true(all(sp_gps$trip_number %in% 1:4))
    
    my_dim <- dim(sp_gps)[1]
    expect_equal(my_dim, 287078)

    expect_true(all(sp_gps$dist >= units::set_units(0, "m")))
    expect_true(all(sp_gps$cumdist >= units::set_units(0, "m")))
    expect_equal(length(sp_gps$speed > units::set_units(0, "km/h")), 287078)
    expect_equal(length(sp_gps$cumtime > units::set_units(0, "s")), 287078)
    
    # messages when gtfs2gps cannot convert all shapes nor all trips
    gtfs <- read_gtfs(sp) %>%
      filter_by_shape_id(52000:52200) %>%
      filter_week_days() %>%
      filter_single_trip()
    
    gtfs$stop_times <- gtfs$stop_times[-(300:390), ]
    expect_warning(result <- gtfs2gps(gtfs, parallel = TRUE, spatial_resolution = 15),
                   "Shape '52200' has zero stops. Ignoring it.")
})
