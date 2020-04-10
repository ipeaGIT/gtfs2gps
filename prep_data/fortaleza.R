
require(gtfs2gps)
require(dplyr)

local_gtfs_path <- "C:/Users/pedro/Dropbox/pesquisa/2019/IPEA/GTFS_fortaleza_20190614.zip"

gtfs <- read_gtfs(local_gtfs_path)

gtfs_shapes_as_sf(gtfs) %>%
  sf::st_geometry() %>%
  plot()

unique(gtfs$shapes$shape_id)

shape_ids <- paste0("shape", c(800:822, 825:854, 856:900), "-I")

new_gtfs <- filter_by_shape_id(gtfs, shape_ids)

unique(new_gtfs$shapes$shape_id)

gtfs_shapes_as_sf(new_gtfs) %>%
  sf::st_geometry() %>%
  plot()

unique(new_gtfs$shapes$shape_id) %>% length()

write_gtfs(new_gtfs, "inst/extdata/fortaleza.zip")
