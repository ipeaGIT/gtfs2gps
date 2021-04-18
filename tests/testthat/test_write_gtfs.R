test_that("write_gtfs", {
    poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

    write_gtfs(poa, "poa2.zip")
    
    poa <- read_gtfs("poa2.zip")
    
    expect_type(poa, "list")
    expect_equal(length(poa), 7)
    
    expect_true(length(poa$agency) >= 1)
    expect_equal(length(poa$routes), 3)
    expect_equal(length(poa$stops), 3)
    expect_equal(length(poa$stop_times), 5)
    expect_equal(length(poa$shapes), 4)
    expect_equal(length(poa$trips), 4)
    expect_equal(length(poa$calendar), 10)
    
    expect_equal(dim(poa$shapes)[1], 1265)
    expect_equal(dim(poa$trips)[1], 387)
    
    invisible(file.remove("poa2.zip"))
    
    # test with frequencies
    sp <- read_gtfs(system.file("extdata/saopaulo.zip", package="gtfs2gps"))
    
    write_gtfs(sp, "sp2.zip")
    
    sp <- read_gtfs("sp2.zip")
    
    expect_type(sp, "list")
    expect_equal(length(sp), 8)
    
    expect_equal(length(sp$agency), 5)
    expect_equal(length(sp$routes), 3)
    expect_equal(length(sp$stops), 3)
    expect_equal(length(sp$stop_times), 5)
    expect_equal(length(sp$shapes), 4)
    expect_equal(length(sp$trips),4)
    expect_equal(length(sp$calendar), 10)
    
    expect_equal(dim(sp$shapes)[1], 35886)
    expect_equal(dim(sp$trips)[1], 92)
    
    invisible(file.remove("sp2.zip"))
})
