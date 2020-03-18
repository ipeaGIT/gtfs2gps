
library(gtfs2gps)
library(dplyr)

fortaleza <- read_gtfs(system.file("extdata/fortaleza.zip", package="gtfs2gps"))
names(fortaleza)

ids <- fortaleza$shapes$shape_id %>% unique()

fortaleza <- filter_by_shape_id(fortaleza, ids[1:2])

for_small_sf <- gtfs_shapes_as_sf(fortaleza)

for_gps <- gtfs2gps(fortaleza, parallel = TRUE)

for_gps_sf <- gps_as_sf(for_gps)
for_gps_small <- for_gps[1:60, ]

for_gps_small_sf <- gps_as_sf(for_gps_small)

plot(sf::st_geometry(for_gps_small_sf), pch = 20)
plot(sf::st_geometry(for_small_sf), col = "blue", add = TRUE)
plot(sf::st_geometry(for_gps_sf), add = TRUE, pch = 20)
box()
