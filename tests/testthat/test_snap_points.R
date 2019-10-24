context("Snap")

test_that("snap_points", {
  gtfs <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps")) %>%
    filter_by_shape_id("T2-1")
  
  gtfs$stops <- gtfs$stops[1,]
  gtfs$shapes <- gtfs$shapes[143:153,]
  
  shapes <- gtfs_shapes_as_sf(gtfs)
  stops <- gtfs_stops_as_sf(gtfs)

  shape_points <- shapes %>% sf::st_coordinates()
  shape_points <- shape_points[, 1:2] %>% as.data.frame()
  shape_points <- sf::st_as_sf(shape_points, coords = c('X', 'Y'), agr = "identity", crs = 31982)

  res <- 15
  result <- cpp_snap_points(stops %>% sf::st_coordinates(), stops %>% sf::st_coordinates(), res)
  
  expect_equal(result, 1)

  expect_error(cpp_snap_points(stops %>% sf::st_coordinates(), matrix(2, 1), res), "Could not find a nearest point closer than 245m.")

  # complete test
  gtfs <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps")) %>%
    filter_by_shape_id("T2-1")

  shapes <- gtfs_shapes_as_sf(gtfs)
  stops <- gtfs_stops_as_sf(gtfs)
  
  shape_points <- shapes %>% sf::st_coordinates()
  shape_points <- shape_points[,1:2] %>% as.data.frame()
  shape_points <- sf::st_as_sf(shape_points, coords = c('X', 'Y'), agr="identity", crs = 31982)
  
  res <- 3000
  result <- cpp_snap_points(stops %>% sf::st_coordinates(), shape_points %>% sf::st_coordinates(), res)
  
  expect_equal(length(result), 62)
  expect_true(all(result == sort(result)))
})
