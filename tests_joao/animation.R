library(gtfs2gps)
library(magrittr)
library(ggplot2)
library(gganimate)
library(data.table)
library(ggthemes)
library(sf)
library(gtfs2gps)

# poa <- system.file("extdata/poa.zip", package="gtfs2gps" )
# poa15 <- gtfs2gps(poa, progress = T, spatial_resolution = 15)
# poa30 <- gtfs2gps(poa, progress = T, spatial_resolution = 30)
# poa60 <- gtfs2gps(poa, progress = T, spatial_resolution = 60)

# nrow(poa15) == nrow(poa30) 
# nrow(poa30) == nrow(poa60)

# Generate GPS-like data
spo <- system.file("extdata/saopaulo.zip", package ="gtfs2gps" )
system.time(spo_gps60 <- gtfs2gps(spo, spatial_resolution = 60, progress = T, cores = getDTthreads()))
table(spo_gps60$trip_id)

# # get static network
# # spo_sf <- gtfs_shapes_as_sf(spo) ?????????????
# network <- spo_gps[, .(trip_id=trip_id[1L], shape_pt_lon=shape_pt_lon[1L], shape_pt_lat =shape_pt_lat[1L] ), by= .(shape_id, id) ]
# network_sf <- gps_as_sf(network)
# ggplot() + geom_sf(data=network_sf[1,], aes(color=shape_id) )


# dt <- subset(spo_gps, trip_id %in% c('176-1@1#1028' , 'A141-1@1#1750', 'R10-2@1#1010'))
# dt <- subset(spo_gps, trip_id %in% unique(spo_gps$trip_id)[1:3] )
# dt <- subset(spo_gps, trip_id %in% c('176-1@1#1028' , 'A141-1@1#1750', 'R10-2@1#1010'))
dt <- spo_gps60[ between(departure_time, lower = as.ITime("07:00:00"), upper = as.ITime("07:10:00")) ]




dt_sf <- gps_as_sf(dt)




p <- ggplot(data = dt, 
            aes(x = shape_pt_lon, y=shape_pt_lat, colour = trip_id)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d()















break()


dt1 <- dt[1:10000,]
anim <- ggplot()+
  geom_point(data = dt1, aes(x = shape_pt_lon, y=shape_pt_lat,color=trip_id),
             show.legend = FALSE, alpha = 0.7,size= 1.5)+
  scale_x_continuous(limits = sf::st_bbox(map_tiles)[c(1,3)] %>% as.vector())+
  scale_y_continuous(limits = sf::st_bbox(map_tiles)[c(2,4)] %>% as.vector())+
  scale_color_viridis_d()+
  # Here comes the gganimate specific bits
  labs(title = 'Time: {frame_time}') +
  exit_fade(alpha = 0)+
  transition_time(as.POSIXct(departure_time,tz=Sys.timezone())) +
  shadow_wake(wake_length=0.01,size=10,colour = "blue", alpha = FALSE) +  #  small wake after data by showing the latest frames up to the current.
  ease_aes('linear') +
  shadow_mark(past = TRUE, future = TRUE, 
              color = 'grey77', size = 0.0, alpha = 1)+ # show the raw data behind the current frame
  theme_map()
             
animate(anim,duration = 5, fps = 20, width = 400, height = 400, renderer = gifski_renderer())
anim_save("./tests_joao/gif_spo_ponto_1-10000.gif")


#
#
 # try to add raster
############################ PONTO speed
map_tiles <- readr::read_rds("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission/data-raw/map_tiles_ceramic/map_tile_crop_ceramic_spo.rds")
map_tiles1 = raster(extent(map_tiles), resolution = 1000,
                    crs = st_crs(3857)$proj4string)
map_tiles1 <- setValues(map_tiles1, 1:ncell(map_tiles1))

map_tiles2 <- projectRaster(from = map_tiles1,crs=st_crs(4326)$proj4string)
map_tiles3 <- as.data.frame(map_tiles2)

ggplot()+
  geom_point(data = dt1, aes(x = shape_pt_lon, y=shape_pt_lat,color=trip_id),
             show.legend = FALSE, alpha = 0.7,size= 1.5)+
  geom_raster(data = map_tiles3, aes(x, y), alpha = 1) 