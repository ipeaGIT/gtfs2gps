
########## LOCAL -------------------------

library(gtfs2gps)
library(gtfstools)
library(data.table)



data_path <- system.file("extdata/spo_gtfs.zip", package = "gtfstools")
system.time(gps <- gtfs2gps(gtfs_data = data_path, spatial_resolution = 500))

fwrite(gps, 'gps_local.csv')
head(gps)


479.64  seconds



##### CRAN --------------------
utils::remove.packages('gtfs2gps')


 install.packages('gtfs2gps')


library(gtfs2gps)
library(data.table)

list.files(system.file("extdata", package = "gtfstools"))

gtfs2 <- gtfstools::read_gtfs(system.file("extdata/spo_gtfs.zip", package = "gtfstools"))

gtfs2 <- gtfstools::frequencies_to_stop_times(gtfs2)


gtfs2$stop_times[,arrival_time := data.table::as.ITime(arrival_time)]
gtfs2$stop_times[,departure_time := data.table::as.ITime(departure_time)]

system.time(sp_gps <- gtfs2gps(gtfs2, spatial_resolution = 500))


fwrite(sp_gps, 'gps_cran.csv')



### comparison --------------------------


# cran 1099.93 seconds
# 
# local 479.64  seconds

c <- fread('gps_cran.csv')
l <- fread('gps_local.csv')


summary(c$speed)
summary(l$speed)

> range de velocidades igual
> local com bem menos speed NA


nrow(c)
2606685

nrow(l)
2609491

> local com um pouco de pontos a mais

unique(c$trip_id) |> length()
7803

unique(l$trip_id) |> length()
7805

> local com um 2 trip ids a mais, MAS mesno numero de shapes



unique(c$shape_id) |> length()
35
unique(l$shape_id) |> length()
35