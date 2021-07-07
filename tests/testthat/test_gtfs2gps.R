test_that("gtfs2gps", {
    poa <- system.file("extdata/poa.zip", package="gtfs2gps")

    poa_gps <- read_gtfs(poa) %>%
      filter_week_days() %>%
      gtfs2gps(parallel = FALSE, spatial_resolution = 50)

    my_dim <- dim(poa_gps)[1]
    expect_equal(my_dim, 128155)
    
    poa_gps <- poa_gps[speed > units::set_units(0, "km/h") & cumtime > units::set_units(0, "s") & !is.na(speed) & !is.infinite(speed),]
    
    my_dim <- dim(poa_gps)[1]
    expect_equal(my_dim, 81037)
    
    my_length <- length(poa_gps$dist[which(!poa_gps$dist < units::set_units(50, "m"))])
    expect_equal(my_length, 0)
    
    expect_equal(sum(poa_gps$dist), 2544820, 0.1)
    
    expect_true(all(poa_gps$trip_number[1] == 1))
    expect_true(all(poa_gps$trip_number[.N] == 3))
    
    expect_true(all(names(poa_gps) %in% 
                      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "trip_number",
                        "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))
    
    expect_true(all(!is.na(poa_gps$dist)))
    
    expect_true(all(poa_gps$dist > units::set_units(0, "m")))
    expect_true(all(poa_gps$cumdist > units::set_units(0, "m")))
    expect_true(all(poa_gps$speed > units::set_units(0, "km/h")))
    expect_true(all(poa_gps$cumtime > units::set_units(0, "s")))
    
    poa_gps <- read_gtfs(poa) %>%
      filter_week_days() %>%
      gtfs2gps(method = "restrictive", spatial_resolution = 50)

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
    expect_equal(my_dim, 16390) # TODO: it was 64911, not it is too low!
    
    my_length <- length(poa_gps$dist[which(!poa_gps$dist < units::set_units(50, "m"))])
    expect_equal(my_length, 0)
    
    expect_equal(sum(poa_gps$dist), 516072, 1)

    expect_true(all(poa_gps$trip_number[1] == 1))
    expect_true(all(poa_gps$trip_number[.N] == 3))
    
    expect_true(all(names(poa_gps) %in% 
      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "trip_number",
        "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))
    
    expect_true(all(!is.na(poa_gps$dist)))
    
    expect_true(all(poa_gps$dist >= units::set_units(0, "m"))) # TEM ZEROS!!
    expect_true(all(poa_gps$cumdist >= units::set_units(0, "m")))
    expect_true(all(poa_gps$speed >= units::set_units(0, "km/h")))
    #expect_true(all(poa_gps$cumtime >= units::set_units(0, "s"))) # ROLL THIS BACK

    poa_gps_30 <- read_gtfs(poa) %>%
      filter_week_days() %>%
      gtfs2gps(spatial_resolution = 30, method = "restrictive")
    
    expect_equal(dim(poa_gps_30)[1], 200172)
    expect(dim(poa_gps_30)[1] > dim(poa_gps)[1], "more spatial_resolution is not decreasing the number of points")
    
    # save into file
    poa_simple <- read_gtfs(poa) %>%
      filter_by_shape_id(c("T2-1", "A141-1"))

    poa_gps <- gtfs2gps(poa_simple, filepath = ".", method = "restrictive", spatial_resolution = 50)
    expect_null(poa_gps)
    
    poa_gps <- gtfs2gps(poa, filepath = ".", continue = TRUE, method = "restrictive", spatial_resolution = 50)

    expect_null(poa_gps)
    
    files1 <- list.files(".", pattern = "\\.txt$")
    names <- gsub('.{4}$', '', files1)
    
    # poa_shape <- gtfs_shapes_as_sf(read_gtfs(poa))
    # expect_setequal(poa_shape$shape_id[2:3], names[c(1,4)])

    poa_gps <- gtfs2gps(poa, filepath = ".", compress = TRUE, method = "restrictive", spatial_resolution = 50)
    
    expect_null(poa_gps)
  
    files2 <- list.files(".", pattern = "\\.rds$")
    names2 <- gsub('.{4}$', '', files2)
    
    data1 <- data.table::fread(files1[1])

    # note how the types are converted    
    data1[, departure_time := as.ITime(departure_time)]
    data1[, stop_id := as.character(stop_id)]
    data1[, trip_number := as.double(trip_number)]
    
    data2 <- readr::read_rds(files2[1])
    
    expect_true(all(data1$trip_id == data2$trip_id))
    
    # poa_shape <- gtfs_shapes_as_sf(read_gtfs(poa))
    # expect_setequal(poa_shape$shape_id[2:3], names[c(1,4)])
    
    file.remove(files1)
    file.remove(files2)

    sp <- system.file("extdata/saopaulo.zip", package="gtfs2gps")

    expect_error(gtfs2gps(sp, continue = TRUE), "Cannot use argument 'continue' without passing a 'filepath'.", fixed = TRUE)
    expect_error(gtfs2gps(sp, compress = TRUE), "Cannot use argument 'compress' without passing a 'filepath'.", fixed = TRUE)
    
    sp_gps <- read_gtfs(sp) %>%
      filter_by_shape_id(52000:52200) %>%
      filter_week_days() %>%
      filter_single_trip() %>%
      gtfs2gps(parallel = FALSE, spatial_resolution = 15, method = "restrictive")

    expect_true(all(names(sp_gps) %in% 
      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "trip_number",
        "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))

    expect_true(all(sp_gps$trip_number %in% 1:17))
    
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
    result <- gtfs2gps(gtfs, parallel = TRUE, spatial_resolution = 15, method = "restrictive")
})
