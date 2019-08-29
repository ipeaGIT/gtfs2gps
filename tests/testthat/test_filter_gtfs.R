context("Filter")

test_that("filter_by_shape_id", {
    poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

    expect_equal(poa$shapes$shape_id %>% unique() %>% length(), 4)

    subset <- filter_by_shape_id(poa, c("A141-1", "T2-1"))
    expect_equal(subset$shapes$shape_id %>% unique() %>% length(), 2)
})

test_that("filter_valid_stop_times", {
  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
  
  expect_equal(dim(poa$stop_times)[1], 774)
  
  subset <- filter_valid_stop_times(poa)
  expect_equal(dim(subset$stop_times)[1], 774)
})

test_that("filter_week_days", {
  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

  expect_equal(dim(poa$trips)[1], 387)
  
  subset <- filter_week_days(poa)
  expect_equal(dim(subset$trips)[1], 194)
})
