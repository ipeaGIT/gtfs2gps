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

  res <- 300
  result <- cpp_snap_points(stops %>% sf::st_coordinates(), shape_points %>% sf::st_coordinates(), res) %>%
    sf::st_as_sf(coords = c('x', 'y'), agr="identity", crs = 31982)
  
  #plot(sf::st_geometry(shapes), pch=20)
  #plot(sf::st_geometry(stops), pch=1, add=T)
  #plot(sf::st_geometry(result), pch=1, col="red", add=T)
  
  expect_equal(sf::st_coordinates(result)[,1], -51.18511, 0.1)
  expect_equal(sf::st_coordinates(result)[,2], -30.0626, 0.1)
  
  # complete test

  gtfs <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps")) %>%
    filter_by_shape_id("T2-1")

  shapes <- gtfs_shapes_as_sf(gtfs)
  stops <- gtfs_stops_as_sf(gtfs)
  
  shape_points <- shapes %>% sf::st_coordinates()
  shape_points <- shape_points[,1:2] %>% as.data.frame()
  shape_points <- sf::st_as_sf(shape_points, coords = c('X', 'Y'), agr="identity", crs = 31982)
  
  result <- cpp_snap_points(stops %>% sf::st_coordinates(), shape_points %>% sf::st_coordinates(), res) %>%
    sf::st_as_sf(coords = c('x', 'y'), agr="identity", crs = 31982)
  
  expect_equal(dim(result)[1], 62)
  
  #shapes %>% write_sf("shapes.shp")
  #result %>% write_sf("result.shp")
  
  coords <- shapes$geometry %>% sf::st_coordinates()
  mresult <- result$geometry %>% sf::st_coordinates()
  
  expect_true(all(mresult[, 1] %in% coords[, 1] & mresult[, 2] %in% coords[, 2]))
})
