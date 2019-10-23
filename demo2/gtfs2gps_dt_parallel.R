
require(gtfs2gps)
require(dplyr)

gtfszip <- system.file("extdata/poa.zip", package = "gtfs2gps")
system.time(test <- gtfs2gps_dt_parallel(gtfszip, spatial_resolution = 50))

test %>% dim()

system.time(test2 <- gtfs2gps_dt_parallel(gtfszip, spatial_resolution = 100))

test2 %>% dim()

# p <- profvis::profvis( test <- gtfs2gps_dt(gtfs) )
