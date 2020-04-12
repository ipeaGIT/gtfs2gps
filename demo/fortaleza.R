
library(gtfs2gps)
library(dplyr)

fortaleza <- read_gtfs(system.file("extdata/fortaleza.zip", package="gtfs2gps"))
names(fortaleza)

ids <- fortaleza$shapes$shape_id %>% unique()
ids

fortaleza <- filter_by_shape_id(fortaleza, ids[1:4])

for_small_sf <- gtfs_shapes_as_sf(fortaleza)

srtmfile <- system.file("extdata/fortaleza-srtm.tif", package = "gtfs2gps")

for_gps <- gtfs2gps(fortaleza, parallel = TRUE) %>% append_height(srtmfile)

for_gps_sf <- gps_as_sf(for_gps)

#write_sf(for_gps_sf, "for-gps2.shp")

for_gps_small <- for_gps

for_gps_small_sf <- gps_as_sf(for_gps_small)

plot(sf::st_geometry(for_gps_small_sf), pch = 20)
plot(sf::st_geometry(for_small_sf), col = "blue", add = TRUE)
plot(sf::st_geometry(for_gps_sf), add = TRUE, pch = 20)
box()

# plotting GPS points with height

srtmraster <- raster::raster(srtmfile)

XYData<- data.frame(
  X = for_gps$shape_pt_lon,
  Y = for_gps$shape_pt_lat)

plot(srtmraster)
points(XYData, type = "p")

plot(for_gps_sf["height"], pch = 20)
