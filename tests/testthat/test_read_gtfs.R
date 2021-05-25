test_that("read_gtfs", {
  expect_error(read_gtfs("xyz123.zip"))
  
  poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
  
  expect_type(poa, "list")
  expect_equal(length(poa), 7)
  
  expect_true(length(poa$agency) >= 1)
  expect_equal(length(poa$routes), 3)
  expect_equal(length(poa$stops), 3)
  expect_equal(length(poa$stop_times), 5)
  expect_equal(length(poa$shapes), 4)
  expect_equal(length(poa$trips), 4)
  expect_equal(length(poa$calendar), 10)
  
  expect_type(poa$stop_times$arrival_time, "integer")
  expect_type(poa$stop_times$departure_time, "integer")
  
  expect_equal(dim(poa$stop_times)[1], 23040)
  expect_equal(dim(poa$shapes)[1], 1265)
  expect_equal(dim(poa$trips)[1], 387)
  
  sp <- read_gtfs(system.file("extdata/saopaulo.zip", package="gtfs2gps"))
  
  expect_type(sp$frequencies$start_time, "integer")
  expect_type(sp$frequencies$end_time, "integer")
  
  file.copy(system.file("extdata/poa.zip", package="gtfs2gps"), "poa.zip")
  
  unzip("poa.zip")
  
  # the last three will be ignored in the tests but need to be removed
  files <- c("stops.txt", "stop_times.txt", "shapes.txt", "trips.txt", "calendar.txt", "agency.txt", "routes.txt")
  
  # for(i in 1:(length(files) - 3)){
  #   if(file.exists("myfile.zip")) file.remove("myfile.zip")
  #   zip("myfile.zip", files[-i], flags = "-q")
  #   expect_error(read_gtfs("myfile.zip"))
  # }
  # 
  # file.remove("myfile.zip")
  # file.remove(files)
  # 
  # empty_files <- c("stops.txt", "stop_times.txt", "shapes.txt", "trips.txt")
  # 
  # for(i in empty_files){
  #   unzip("poa.zip")
  #   if(file.exists("myfile.zip")) file.remove("myfile.zip")
  #   file.remove(i)
  #   file.create(i)
  #   zip("myfile.zip", files, flags = "-q")
  #   
  #   expect_error(expect_warning(read_gtfs("myfile.zip", quiet = TRUE)))
  # }
  # 
  # file.remove("myfile.zip")
  # file.remove(files)
  # file.remove("poa.zip")
})
