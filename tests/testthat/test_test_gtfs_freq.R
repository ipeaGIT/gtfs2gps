test_that("test_gtfs_freq", {
  
  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
  expect_equal(test_gtfs_freq(poa), "simple")
  
  spo <- read_gtfs(system.file("extdata/saopaulo.zip", package="gtfs2gps"))
  expect_equal(test_gtfs_freq(spo), "frequency")
  
})