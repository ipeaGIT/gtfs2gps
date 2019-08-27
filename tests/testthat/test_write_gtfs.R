context("Write")

test_that("write_gtfs", {
    poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

    write_gtfs(poa, "poa2.zip")
    
    poa <- read_gtfs("poa2.zip")
    
    expect_type(poa, "list")
    expect_equal(length(poa), 7)
    
    expect_equal(length(poa$agency), 7)
    expect_equal(length(poa$routes), 9)
    expect_equal(length(poa$stops), 6)
    expect_equal(length(poa$stop_times), 5)
    expect_equal(length(poa$shapes), 4)
    expect_equal(length(poa$trips), 10)
    expect_equal(length(poa$calendar), 10)
    
    expect_equal(dim(poa$shapes)[1], 1265)
    expect_equal(dim(poa$trips)[1], 387)
    
    invisible(file.remove("poa2.zip"))
})
