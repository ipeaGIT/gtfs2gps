
require(gtfs2gps)
require(dplyr)

local_gtfs_path <- "C:/Users/pedro/Dropbox/pesquisa/2019/IPEA/GTFS_POA_20190415.zip"

gtfs <- read_gtfs(local_gtfs_path)

shape_ids <- c("T2-1", "176-1", "A141-1", "R10-2")

new_gtfs <- filter_by_shape_id(gtfs, shape_ids) %>%
  filter_valid_stop_times()

write_gtfs(new_gtfs, "inst/extdata/poa.zip")
