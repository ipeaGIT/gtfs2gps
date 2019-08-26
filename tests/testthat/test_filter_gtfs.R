context("Filter")

test_that("filter_by_shape_id", {
    poa <- tidytransit::read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"),
                                  local = TRUE,
                                  geometry = TRUE,
                                  frequency = TRUE)

    expect_equal(poa$shapes$shape_id %>% unique() %>% length(), 4)

    subset <- filter_by_shape_id(poa, c("A141-1", "T2-1"))
    expect_equal(subset$shapes$shape_id %>% unique() %>% length(), 2)
})

test_that("filter_valid_stop_times", {
  poa <- tidytransit::read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"),
                                local = TRUE,
                                geometry = TRUE,
                                frequency = TRUE)
  
  expect_equal(dim(poa$stop_times)[1], 774)
  
  subset <- filter_valid_stop_times(poa)
  expect_equal(dim(subset$stop_times)[1], 774)
})
