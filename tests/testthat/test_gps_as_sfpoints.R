
test_that("gps_as_sfpoints", {
  fortaleza <- read_gtfs(system.file("extdata/fortaleza.zip", package = "gtfs2gps"))
  srtmfile <- system.file("extdata/fortaleza-srtm.tif", package="gtfs2gps")
  
  subset <- fortaleza |>
    gtfstools::filter_by_weekday(c("monday", "tuesday", "wednesday", "thursday", "friday")) |>
    filter_single_trip() |>
    gtfstools::filter_by_shape_id(c("shape804-I", "shape806-I"))
  
  for_gps <- gtfs2gps(subset)
  for_gps_sf_points <- gps_as_sfpoints(for_gps) # without height
  coords <- for_gps_sf_points |> sf::st_coordinates()
  
  expect_true(is(for_gps_sf_points, "sf"))
  expect_equal(dim(coords)[2], 2)
    
  for_gps <- append_height(for_gps, srtmfile)
  for_gps_sf_points <- gps_as_sfpoints(for_gps) # with height
  coords <- for_gps_sf_points |> sf::st_coordinates()
  
  expect_equal(dim(coords)[2], 3)
})
