
test_that("gps_as_sflinestring", {
  fortaleza <- read_gtfs(system.file("extdata/fortaleza.zip", package = "gtfs2gps"))

  subset <- fortaleza |>
    gtfstools::filter_by_weekday(c("monday", "tuesday", "wednesday", "thursday", "friday")) |>
    filter_single_trip() |>
    gtfstools::filter_by_shape_id(c("shape804-I", "shape806-I"))
  
  for_gps <- gtfs2gps(subset) |> adjust_speed()

  for_gps_sf_lines <- gps_as_sflinestring(for_gps)
  for_gps_sf_lines
  expect_true(is(for_gps_sf_lines, "sf"))

  expect_equal(dim(for_gps_sf_lines)[1], 28)
  expect_equal(for_gps_sf_lines$from_stop_id[1], "2649")
  expect_equal(length(names(for_gps_sf_lines)), 15)
  
  # -----
  # test to fix NA values
  gps <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps")) |>
    gtfstools::filter_by_shape_id(.,"176-1") |> 
    filter_single_trip() |> 
    gtfs2gps() |> 
    adjust_speed() |> 
    gps_as_sflinestring()
  expect_equal(sum(units::drop_units(gps$cumtime)), 6248,tolerance = 0.1)
  
})
