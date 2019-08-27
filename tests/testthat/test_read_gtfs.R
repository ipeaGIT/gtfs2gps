context("Read")

test_that("read_gtfs", {
    poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

    expect_type(poa, "list")
    expect_equal(length(poa), 6)
    
    expect_equal(length(poa$routes), 9)
    expect_equal(length(poa$stops), 6)
    expect_equal(length(poa$stoptimes), 5)
    expect_equal(length(poa$shapes), 4)
    expect_equal(length(poa$trips), 10)
    expect_equal(length(poa$calendar), 10)
})
