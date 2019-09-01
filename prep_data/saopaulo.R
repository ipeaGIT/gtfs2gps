
require(gtfs2gps)
require(dplyr)

local_gtfs_path <- "C:/Users/pedro/Dropbox/pesquisa/2019/IPEA/GTFS Sptrans_20190619.zip"

gtfs <- read_gtfs(local_gtfs_path)

#unique(gtfs$shapes$shape_id)

shape_ids <- 50000:55000

new_gtfs <- filter_by_shape_id(gtfs, shape_ids)

gtfs_shapes_as_sf(new_gtfs) %>%
  sf::st_geometry() %>%
  plot()

unique(new_gtfs$shapes$shape_id) %>% length()

write_gtfs(new_gtfs, "inst/extdata/saopaulo.zip")
