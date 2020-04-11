
test_that("append_height", {
  fortaleza <- system.file("extdata/fortaleza.zip", package="gtfs2gps")
  srtmfile <- system.file("extdata/fortaleza-srtm.tif", package="gtfs2gps")
  
  gtfs <- read_gtfs(fortaleza) %>%
    filter_week_days() %>%
    filter_single_trip()
  
  fortaleza_gps <- gtfs2gps(gtfs, progress = FALSE) %>% append_height(srtmfile)

  expect_equal(sum(fortaleza_gps$height), 72932, 0.05)
  
  fort <- fortaleza_gps[which(is.na(fortaleza_gps$height)),]
  
  #fott <- gps_as_sf(fort)
  #sf::write_sf(fott, "fort-na.shp")  
})
