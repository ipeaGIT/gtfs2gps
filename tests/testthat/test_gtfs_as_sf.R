context("gtfs_as_sf")

test_that("gtfs_shape_as_sf", {
    poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

    poa_sf <- gtfs_shapes_as_sf(poa)
    
    expect_true(inherits(poa_sf, "sf"))
    expect_equal(dim(poa_sf)[1], 4)
    
    expect_true("length" %in% names(poa_sf))
})

test_that("gtfs_stops_as_sf", {
  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
  
  poa_sf <- gtfs_stops_as_sf(poa)
  
  expect_true(inherits(poa_sf, "sf"))
  expect_equal(dim(poa_sf)[1], 212)
})
