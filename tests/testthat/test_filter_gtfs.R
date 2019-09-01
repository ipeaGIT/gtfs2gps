context("Filter")

test_that("filter_by_shape_id", {
    poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

    expect_equal(poa$shapes$shape_id %>% unique() %>% length(), 4)

    subset <- filter_by_shape_id(poa, c("A141-1", "T2-1"))
    expect_equal(subset$shapes$shape_id %>% unique() %>% length(), 2)

    prev_stop_times <- dim(subset$stop_times)[1]
    prev_stops <- dim(subset$stops)[1]
    prev_routes <- dim(subset$routes)[1]
    
    unique_shape_ids <- subset$shapes$shape_id %>% unique()
    subset <- gtfs2gps::filter_by_shape_id(subset, unique_shape_ids)

    expect_equal(dim(subset$stop_times)[1], prev_stop_times)
    expect_equal(dim(subset$stops)[1], prev_stops)
    expect_equal(dim(subset$routes)[1], prev_routes)
    
})

test_that("filter_valid_stop_times", {
  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
  
  expect_equal(dim(poa$stop_times)[1], 23040)
  expect_equal(dim(poa$stops)[1], 212)
  expect_equal(dim(poa$routes)[1], 4)
  
  subset <- filter_valid_stop_times(poa)
  expect_equal(dim(subset$stop_times)[1], 774)
  expect_equal(dim(subset$stops)[1], 8)
  expect_equal(dim(subset$routes)[1], 4)
})

test_that("filter_week_days", {
  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

  expect_equal(dim(poa$trips)[1], 387)
  
  subset <- filter_week_days(poa)
  expect_equal(dim(subset$trips)[1], 194)
  expect_equal(sum(subset$calendar$sunday), 0)
  expect_equal(sum(subset$calendar$saturday), 0)
})
