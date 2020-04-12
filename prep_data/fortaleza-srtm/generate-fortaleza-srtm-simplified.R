# SRTM data downloaded from
# http://www.dsr.inpe.br/topodata/acesso.php

require(raster)
require(gtfs2gps)

inputDir <- "prep_data/fortaleza-srtm/"

myraster <- paste0(inputDir, "03S39_ZN.tif") %>% raster::raster()

fortaleza <- read_gtfs(system.file("extdata/fortaleza.zip", package="gtfs2gps"))

fortaleza_sf <- gtfs_shapes_as_sf(fortaleza) %>% sf::st_buffer(0.001) %>% sf::as_Spatial()

mm.sub <- raster::crop(myraster, fortaleza_sf)

raster::writeRaster(mm.sub, "inst/extdata/fortaleza-srtm.tif", overwrite = TRUE)
               