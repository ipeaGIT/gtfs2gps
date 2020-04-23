test_that("gps_as_sf", {
  poa <- system.file("extdata/poa.zip", package="gtfs2gps")
  
  poa_gps_sf <- gps_as_sf(gtfs2gps(poa, parallel = FALSE, progress = FALSE))
  
  expect_true(is(poa_gps_sf, "sf"))
})


test_that("gps_as_sf_points", {
  fortaleza <- read_gtfs(system.file("extdata/fortaleza.zip", package = "gtfs2gps"))
  srtmfile <- system.file("extdata/fortaleza-srtm.tif", package="gtfs2gps")
  
  subset <- fortaleza %>%
    filter_week_days() %>%
    filter_single_trip() %>%
    filter_by_shape_id(c("shape804-I", "shape806-I"))
  
  for_gps <- gtfs2gps(subset)
  for_gps_sf_points <- gps_as_sf_points(for_gps) # without height
  coords <- for_gps_sf_points %>% sf::st_coordinates()
  
  expect_equal(dim(coords)[2], 2)
    
  for_gps <- append_height(for_gps, srtmfile)
  for_gps_sf_points <- gps_as_sf_points(for_gps) # with height
  coords <- for_gps_sf_points %>% sf::st_coordinates()
  
  expect_equal(dim(coords)[2], 3)
})
