test_that("gtfs2gps", {
    poa <- system.file("extdata/poa.zip", package="gtfs2gps")

    poa_gps_0 <- read_gtfs(poa) |>
      adjust_arrival_departure() |>
      gtfstools::filter_by_weekday(c("monday", "tuesday", "wednesday", "thursday", "friday")) |>
      gtfs2gps(parallel = FALSE, spatial_resolution = 50)

    expect_equal(sum(units::drop_units(poa_gps_0$cumtime), na.rm = TRUE), 212843109, 0.01)
    expect_equal(sum(units::drop_units(poa_gps_0$speed), na.rm = TRUE), 3079200, 0.01)
    expect_equal(sum(units::drop_units(poa_gps_0$cumdist), na.rm = TRUE), 1447649389, 0.01)
    
    my_dim_0 <- dim(poa_gps_0)[1]
    expect_equal(my_dim_0, 128543)
    
    poa_gps <- poa_gps_0[units::drop_units(speed) > 0 & units::drop_units(cumtime) >= 0 & !is.na(speed),]
    
    my_dim <- dim(poa_gps)[1]
    expect_equal(my_dim, 126660)
    
    my_length <- length(poa_gps$dist[which(!units::drop_units(poa_gps$dist) < 50)])
    expect_equal(my_length, 0)
    
    expect_equal(sum(units::drop_units(poa_gps$dist)), 4085722, 0.1)
    
    expect_true(poa_gps$trip_number[1] == 1)
    expect_true(poa_gps$trip_number[126660] == 88)
    
    expect_true(all(names(poa_gps) %in% 
                      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "trip_number",
                        "timestamp", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))
    
    expect_true(all(!is.na(poa_gps$dist)))
    
    expect_true(all(poa_gps$dist >= units::set_units(0, "m")))
    expect_true(all(poa_gps$cumdist >= units::set_units(0, "m")))
    expect_true(all(poa_gps$speed >= units::set_units(0, "km/h")))
    expect_true(all(poa_gps$cumtime >= units::set_units(0, "s")))
    
    poa_gps <- read_gtfs(poa) |>
      gtfstools::filter_by_weekday(c("monday", "tuesday", "wednesday", "thursday", "friday")) |>
      gtfs2gps(spatial_resolution = 50)

    #poa_shape <- read_gtfs(poa) |> gtfs_shapes_as_sf()
    #plot(poa_shape)
    #poa_gps_shape <- gps_as_sf(poa_gps)
    #plot(poa_gps_shape)
    #write_sf(poa_shape, "poa_shape.shp")
    #write_sf(poa_gps_shape, "poa_gps_shape.shp")
    
    my_dim <- dim(poa_gps)[1]
    total <- 128543

    expect_equal(my_dim, total)

    poa_gps <- poa_gps[units::drop_units(speed) > 0 & units::drop_units(cumtime) > 0 & !is.na(speed) & !is.infinite(speed),]

    expect_true(all(na.omit(units::drop_units(poa_gps$speed) > 0)))
    
    my_dim <- dim(poa_gps)[1]
    expect_equal(my_dim, 126272)
    
    my_length <- length(poa_gps$dist[which(!(units::drop_units(poa_gps$dist) < 50))])
    expect_equal(my_length, 0)
    
    expect_equal(sum(units::drop_units(poa_gps$dist)), 516072, 1)

    expect_true(poa_gps$trip_number[1] == 1)
    expect_true(poa_gps$trip_number[126272] == 88)
    
    expect_true(all(names(poa_gps) %in% 
      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "trip_number",
        "timestamp", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))
    
    expect_true(all(!is.na(poa_gps$dist)))
    
    expect_true(all(poa_gps$dist >= units::set_units(0, "m")))
    expect_true(all(poa_gps$cumdist >= units::set_units(0, "m")))
    expect_true(all(poa_gps$speed >= units::set_units(0, "km/h")))
    expect_true(all(poa_gps$cumtime >= units::set_units(0, "s")))

    poa_gps_30 <- read_gtfs(poa) |>
      gtfstools::filter_by_weekday(c("monday", "tuesday", "wednesday", "thursday", "friday")) |>
      gtfs2gps(spatial_resolution = 30)
    
    expect_equal(dim(poa_gps_30)[1], 200560)
    expect(dim(poa_gps_30)[1] > dim(poa_gps)[1], "more spatial_resolution is not decreasing the number of points")
    
    # save into file
    poa_simple <- read_gtfs(poa) |>
      gtfstools::filter_by_shape_id(c("T2-1", "A141-1"))

    poa_gps <- gtfs2gps(poa_simple, filepath = ".", spatial_resolution = 50)
    expect_null(poa_gps)
    
    poa_gps <- gtfs2gps(poa, filepath = ".", continue = TRUE, spatial_resolution = 50)

    expect_null(poa_gps)
    
    files1 <- list.files(".", pattern = "\\.txt$")
    names <- gsub('.{4}$', '', files1)
    
    # poa_shape <- gtfs_shapes_as_sf(read_gtfs(poa))
    # expect_setequal(poa_shape$shape_id[2:3], names[c(1,4)])

    poa_gps <- gtfs2gps(poa, filepath = ".", compress = TRUE, spatial_resolution = 50)
    
    expect_null(poa_gps)
  
    files2 <- list.files(".", pattern = "\\.rds$")
    names2 <- gsub('.{4}$', '', files2)
    
    data1 <- data.table::fread(files1[1])

    # note how the types are converted    
    data1[, timestamp := as.ITime(timestamp)]
    data1[, stop_id := as.character(stop_id)]
    data1[, trip_number := as.double(trip_number)]
    
    data2 <- readRDS(files2[1])
    
    expect_true(all(data1$trip_id == data2$trip_id))
    
    # poa_shape <- gtfs_shapes_as_sf(read_gtfs(poa))
    # expect_setequal(poa_shape$shape_id[2:3], names[c(1,4)])
    
    file.remove(files1)
    file.remove(files2)

    sp <- system.file("extdata/saopaulo.zip", package="gtfs2gps")

    expect_error(gtfs2gps(sp, continue = TRUE), "Cannot use argument 'continue' without passing a 'filepath'.", fixed = TRUE)
    expect_error(gtfs2gps(sp, compress = TRUE), "Cannot use argument 'compress' without passing a 'filepath'.", fixed = TRUE)
    

    # sp <- system.file("extdata/saopaulo.zip", package="gtfs2gps")
    # 
    # sp2 <- gtfstools::read_gtfs(sp)
    # sp3 <- gtfstools::frequencies_to_stop_times(sp2)
    # sp_gps <- sp3 |>
    #   adjust_arrival_departure()
    # 
    # gtfstools::write_gtfs(sp3, "sp3.zip")
    # 
    # sp4 <- read_gtfs("sp3.zip")
    # 
    # sp_gps <- sp4 |>
    #   gtfstools::filter_by_shape_id(52072) |>
    #  gtfstools::filter_by_weekday(c("monday", "tuesday", "wednesday", "thursday", "friday")) |>
    #   filter_single_trip() |>
    #   gtfs2gps(parallel = FALSE, spatial_resolution = 15)
    # 
    # expect_true(all(names(sp_gps) %in%
    #   c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat", "trip_number",
    #     "timestamp", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))
    # 
    # expect_true(all(sp_gps$trip_number %in% 1:17))
    # 
    # my_dim <- dim(sp_gps)[1]
    # total <- 287078
    # if(my_dim %in% c(261935, 265771, 274311)) total <- my_dim # Linux differences
    # 
    # expect_equal(my_dim, total)
    # 
    # expect_true(all(sp_gps$dist >= units::set_units(0, "m")))
    # expect_true(all(sp_gps$cumdist >= units::set_units(0, "m")))
    # expect_equal(length(sp_gps$speed > units::set_units(0, "km/h")), total)
    # expect_equal(length(sp_gps$cumtime > units::set_units(0, "s")), total)
    # 
    # # messages when gtfs2gps cannot convert all shapes nor all trips
    # gtfs <- read_gtfs(sp) |>
    #   gtfstools::filter_by_shape_id(52000:52200) |>
    #  gtfstools::filter_by_weekday(c("monday", "tuesday", "wednesday", "thursday", "friday")) |>
    #   filter_single_trip()
    # 
    # gtfs$stop_times <- gtfs$stop_times[-(300:390), ]
    # result <- gtfs2gps(gtfs, parallel = TRUE, spatial_resolution = 15)

    
})
