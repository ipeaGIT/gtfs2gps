
test_that("gps_as_sflinestring", {
  
  fortaleza <- read_gtfs(system.file("extdata/fortaleza.zip", package = "gtfs2gps"))

  subset <- fortaleza %>%
    filter_week_days() %>%
    filter_single_trip() %>%
    filter_by_shape_id(c("shape804-I", "shape806-I"))
  
  for_gps <- gtfs2gps(subset)

  for_gps_sf_points <- gps_as_sflinestring(for_gps)
  
  expect_true(is(for_gps_sf_points, "sf"))

  expect_equal(for_gps_sf_points$stop_id[1], 2649)
  
})
