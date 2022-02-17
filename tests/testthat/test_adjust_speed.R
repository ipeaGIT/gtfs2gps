test_that("adjust_speed", {
    poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

    poa_gps <- gtfs2gps(poa)

    poa_gps_old <- data.table::copy(poa_gps)
    
    expect_true(any(is.na(poa_gps$speed)))
    expect_true(any(is.na(poa_gps$cumtime)))
    
    expect_equal(mean(units::drop_units(poa_gps$speed), na.rm = TRUE), 24.7198, 0.0001)
    expect_equal(mean(units::drop_units(poa_gps$cumtime), na.rm = TRUE), 1588.508, 0.001)    

    poa_gps_new <- adjust_speed(poa_gps)
    
    expect_true(!any(is.na(poa_gps_new$speed)))
    expect_true(!any(is.na(poa_gps_new$cumtime)))
    
    expect_equal(mean(units::drop_units(poa_gps_new$speed)), 25.05756, 0.0001)
    expect_equal(mean(units::drop_units(poa_gps_new$cumtime)), 1599.597, 0.001)

    poa_gps_new <- adjust_speed(poa_gps, min_speed = 25, max_speed = 50)
    
    expect_true(all(poa_gps_new$speed >= units::set_units(25, "km/h"), na.rm = TRUE))
    expect_true(all(poa_gps_new$speed <= units::set_units(50, "km/h"), na.rm = TRUE))

    expect_equal(mean(units::drop_units(poa_gps_new$speed), na.rm = TRUE), 35.09653, 0.0001)
    expect_equal(mean(units::drop_units(poa_gps_new$cumtime)), 1131.914, 0.001)
})
