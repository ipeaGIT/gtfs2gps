
require(gtfs2gps)

local_gtfs_path <- "C:/Users/pedro/Dropbox/pesquisa/2019/IPEA/GTFS_POA_20190415.zip"

gtfs <- tidytransit::read_gtfs(local_gtfs_path,
                               local = TRUE,
                               geometry = TRUE,
                               frequency = TRUE)

shape_ids <- c("T2-1", "176-1", "A141-1", "R10-2")

new_gtfs <- filter_by_shape_id(gtfs, shape_ids)

new_gtfs$stop_times <- new_gtfs$stop_times %>% filter(!is.na(arrival_time))

plot(new_gtfs)

tidytransit::write_gtfs(new_gtfs, "poa.zip")
