context("Read")

test_that("read_gtfs", {
    poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

    expect_type(poa, "list")
    expect_equal(length(poa), 7)
    
    expect_equal(length(poa$agency), 7)
    expect_equal(length(poa$routes), 9)
    expect_equal(length(poa$stops), 6)
    expect_equal(length(poa$stop_times), 5)
    expect_equal(length(poa$shapes), 4)
    expect_equal(length(poa$trips), 10)
    expect_equal(length(poa$calendar), 10)
    
    expect_type(poa$stop_times$arrival_time, "integer")
    expect_type(poa$stop_times$departure_time, "integer")
    
    expect_equal(dim(poa$stop_times)[1], 774)
    expect_equal(dim(poa$shapes)[1], 1265)
    expect_equal(dim(poa$trips)[1], 387)
    
    sp <- read_gtfs(system.file("extdata/saopaulo.zip", package="gtfs2gps"))

    expect_type(sp$frequencies$start_time, "integer")
    expect_type(sp$frequencies$end_time, "integer")
})
