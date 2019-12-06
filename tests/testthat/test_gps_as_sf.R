test_that("gps_as_sf", {
  poa <- system.file("extdata/poa.zip", package="gtfs2gps")
  
  poa_gps_sf <- gps_as_sf(gtfs2gps(poa, progress = FALSE))
  
  expect_true(is(poa_gps_sf, "sf"))
})
