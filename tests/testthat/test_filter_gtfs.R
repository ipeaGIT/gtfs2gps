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

    sp <- read_gtfs(system.file("extdata/saopaulo.zip", package="gtfs2gps"))

    sp_small <- gtfs2gps::filter_by_shape_id(sp, 53000:53020)
    
    expect(dim(sp$stop_times)[1] > dim(sp_small$stop_times)[1], "Stop_times were not subset")
    expect(dim(sp$stops)[1] > dim(sp_small$stops)[1], "Stops were not subset")
    expect(dim(sp$routes)[1] > dim(sp_small$routes)[1], "Routes were not subset")
    expect(dim(sp$frequencies)[1] > dim(sp_small$frequencies)[1], "Frequencies were not subset")
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
  
  poa$calendar <- NULL
  
  expect_error(filter_week_days(poa), "GTFS data does not have calendar")
})

test_that("filter_by_day", {
  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
  
  subset <- filter_week_days(poa)
  
  subset2 <- filter_by_day(poa, c("monday", "tuesday", "wednesday", "thursday", "friday"))
  
  expect(identical(subset, subset2), "not identical")
  
  berlin <- read_gtfs(system.file("extdata/berlin.zip", package="gtfs2gps"))
  
  subset2 <- filter_by_day(berlin, c("monday", "tuesday", "wednesday", "thursday", "friday"))
  
  expect_equal(length(subset2$calendar_dates), 3)
  expect_equal(dim(subset2$calendar_dates)[1], 28843)

  subset3 <- filter_by_day(subset2, c("monday", "tuesday", "wednesday", "thursday", "friday"))

  expect(identical(subset2, subset3), "not identical")
})

test_that("filter_single_trip", {
  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
  
  expect_equal(dim(poa$trips)[1], 387)
  expect_equal(dim(poa$shapes)[1], 1265)
  
  expect_true(all(poa$trips$shape_id %in% poa$shapes$shape_id))

  subset <- filter_single_trip(poa)

  expect_equal(dim(poa$shapes)[1], 1265)
  expect_equal(dim(subset$trips)[1], length(unique(poa$shapes$shape_id)))
})

test_that("filter_by_agency_id", {
  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
  
  result <- filter_by_agency_id(poa, "EPTC")
  expect_equal(dim(result$trips)[1], 387)
  expect_equal(dim(result$shapes)[1], 1265)
  
  sp <- read_gtfs(system.file("extdata/saopaulo.zip", package="gtfs2gps"))

  result <- filter_by_agency_id(sp, 1)
  expect_equal(dim(result$trips)[1], 233)
  expect_equal(dim(result$shapes)[1], 94386)

  sp$routes <- NULL
  expect_error(filter_by_agency_id(sp, "abc"), "GTFS data does not have routes")
  
  sp$agency <- NULL
  expect_error(filter_by_agency_id(sp, "abc"), "GTFS data does not have agency")
})

test_that("remove_invalid", {
  sp <- read_gtfs(system.file("extdata/saopaulo.zip", package="gtfs2gps"))

  sp$shapes <- sp$shapes[-(1:80000),]
  sp$agency$agency_id <- 'test'
  
  sp2 <- remove_invalid(sp, prompt_invalid = TRUE)
  
  expect_equal(length(sp$stops$stop_id), 6117)
  expect_equal(length(sp2$stops$stop_id), 1340)
  
  sp3 <- remove_invalid(sp, only_essential = FALSE)
  
  expect_equal(length(sp3$stops$stop_id), 0)
})

test_that("filter_by_route_id", {
  warsaw <- read_gtfs(system.file("extdata/warsaw.zip", package="gtfs2gps"))

  expect_equal(dim(warsaw$trips)[1], 56)
  expect_equal(dim(warsaw$shapes)[1], 3075)
    
  subset <- filter_by_route_id(warsaw, c("15", "175"))

  expect(all(subset$routes$route_id %in% c("15", "175")), "invalid route_ids")
  
  expect_equal(dim(subset$trips)[1], 48)
  expect_equal(dim(subset$shapes)[1], 1370)
  
  sp <- read_gtfs(system.file("extdata/saopaulo.zip", package="gtfs2gps"))

  subset <- filter_by_route_id(sp, "N131-11")
  
  expect(all(subset$routes$route_id %in% "N131-11"), "invalid route_ids")
  
  expect_equal(dim(subset$trips)[1], 1)
  expect_equal(dim(subset$shapes)[1], 758)

  sp$routes <- NULL
  expect_error(filter_by_agency_id(sp, "abc"), "GTFS data does not have routes")
})

test_that("filter_by_route_type", {
  warsaw <- read_gtfs(system.file("extdata/warsaw.zip", package="gtfs2gps"))

  subset <- filter_by_route_type(warsaw, c(0, 3))

  expect(all(subset$routes$route_type %in% c(0, 3)), "invalid route_types")
    
  expect_equal(dim(subset$trips)[1], 48)
  expect_equal(dim(subset$shapes)[1], 1370)
  
  sp <- read_gtfs(system.file("extdata/saopaulo.zip", package="gtfs2gps"))
  
  sp$routes$route_type[1] <- 1

  subset <- filter_by_route_type(sp, 1)
  
  expect(all(subset$routes$route_type %in% 1), "invalid route_types")
  
  expect_equal(dim(subset$trips)[1], 1)
  expect_equal(dim(subset$shapes)[1], 583)

  sp$routes <- NULL
  expect_error(filter_by_agency_id(sp, "abc"), "GTFS data does not have routes")
})

