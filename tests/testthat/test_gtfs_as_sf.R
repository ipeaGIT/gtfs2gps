context("gtfs_as_sf")

test_that("gtfs_shape_as_sf", {
    poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

    poa_sf <- gtfs_shapes_as_sf(poa)
    
    expect_true(inherits(poa_sf, "sfc"))
    expect_equal(length(poa_sf), 4)
})
