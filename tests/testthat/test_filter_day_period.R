test_that("filter_day_period", {

  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
  
  # working as expected
    poa1 <- filter_day_period(poa, start = "11:00", end = "12:00")
    expect_equal( length(unique(poa1$stop_times$departure_time)), 30)    

  # invalid inputs

   #  
   #  poa2 <- filter_day_period(poa, start = "00:00", end = "26:00")
   #  
   # identical(poa2, poa)
   # 
   # length(unique(poa2$stop_times$departure_time))
   # length(unique(poa$stop_times$departure_time))
   
})
