test_that("filter_by_shape_id", {
    poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

    poa_gps <- gtfs2gps(poa)

    expect_true(any(is.na(poa_gps$speed)))
    expect_true(any(is.na(poa_gps$cumtime)))
    
    expect_equal(mean(poa_gps$speed, na.rm = TRUE), 24.89832, 0.0001)
    expect_equal(mean(poa_gps$cumtime, na.rm = TRUE), 1622.147, 0.001)    

    poa_gps_new <- estimate_cumtime(poa_gps)
    
    expect_true(!any(is.na(poa_gps_new$speed)))
    expect_true(!any(is.na(poa_gps_new$cumtime)))
    
    expect_equal(mean(poa_gps_new$speed), 24.89832, 0.0001)
    expect_equal(mean(poa_gps_new$cumtime), 0.4473, 0.001)
})
