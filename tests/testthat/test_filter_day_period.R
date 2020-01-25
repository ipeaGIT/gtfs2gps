test_that("filter_day_period", {

  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
  
  # working as expected
    poa1 <- filter_day_period(poa, period_start = "11:00", period_end = "12:00")
    expect_equal( length(unique(poa1$stop_times$departure_time)), 30)    

    poa2 <- filter_day_period(poa, period_end = "12:00")
    expect_equal( length(unique(poa2$stop_times$departure_time)), 206)    
    
    poa3 <- filter_day_period(poa, period_start = "11:00")
    expect_equal( length(unique(poa3$stop_times$departure_time)), 345)
    
   # invalid inputs
    testthat::expect_error(filter_day_period(poa, period_start = "zzz", period_end = "12:00"))
    testthat::expect_error(filter_day_period(poa, period_start = "10:00", period_end = "zzz"))
    })
