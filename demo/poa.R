
library(gtfs2gps)
library(dplyr)

poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps")) %>%
  filter_by_shape_id("T2-1")

names(poa)
sp$trips

poa_shp <- gtfs_shapes_as_sf(poa)
poa_stops <- gtfs_stops_as_sf(poa)

plot(sf::st_geometry(poa_shp))
plot(sf::st_geometry(poa_stops), col = "blue", add = TRUE)
box()

gps <- gtfs2gps(poa, plan = TRUE)

gps_sf <- gps_as_sf(gps) %>%
  dplyr::filter(trip_id == "T2-1@1#520")

plot(sf::st_geometry(gps_sf))
