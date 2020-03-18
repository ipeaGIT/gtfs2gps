library(gtfs2gps)
library(magrittr)
library(ggplot2)
library(gganimate)
library(data.table)
library(ggthemes)
library(sf)
library(viridis)

gc(reset = TRUE)


Sys.setenv(TZ= Sys.timezone(location = T) )

gtfs_zip <- "L:/Proj_acess_oport/data-raw/gtfs/spo/gtfs_spo_sptrans_2019-10.zip"
 
###### Prepare data  ------------------

# local GTFS.zip file
gtfs_zip <- system.file("extdata/saopaulo.zip", package="gtfs2gps" )

# read gtfs
gtfs_dt <- gtfs2gps::read_gtfs(gtfs_zip)

# filter transport services on weekdays
gtfs_dt <- gtfs2gps::filter_week_days(gtfs_dt)


# # # subset time interval
# gtfs_dt <- gtfs2gps::filter_day_period(gtfs_dt, period_start = "07:00:", period_end = "10:00")
  
# get transport network as sf object
shapes_sf <- gtfs_shapes_as_sf(gtfs_dt)

# Convert GTFS data into a data.table with GPS-like records
gps_dt <- gtfs2gps(gtfs_dt, spatial_resolution = 30, progress = T, parallel = T )
head(gps_dt)

# subset time interval
gps_dt2 <- gps_dt[ between(departure_time, as.ITime("07:00:"), as.ITime("07:30"))]
gps_dt2 <- gps_dt2[speed < 20 , ]

# Convert "GPS" points into sf
gps_sf <- sfheaders::sf_multipoint(gps_dt2, x = "shape_pt_lon" , y = "shape_pt_lat", multipoint_id = "shape_id", keep = T)
sf::st_crs(gps_sf) <- 4326

# gps_sf <- gps_as_sf(gps_dt)
head(gps_sf)







# read map tile
map_tiles <- readr::read_rds("L:/Proj_acess_oport/data/map_tiles_crop/ceramic/map_tile_crop_ceramic_spo.rds")


###### Static plot ------------------

# static plot: routes

ggplot() +
  geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
  geom_sf(data= st_transform(shapes_sf, 3857), color='red') +
# theme(legend.position = "none") +
  scale_fill_identity() +
  coord_sf() 






###### .gif plot ------------------

anim <- ggplot() +
          geom_sf(data=shapes_sf, color='gray90', size=0.01) +
          geom_point(data = gps_dt, aes(x = shape_pt_lon, y=shape_pt_lat, colour = speed), size=1.5, alpha = 0.1, show.legend = FALSE) +
          scale_colour_viridis() +
          
          # gganimate specificatons
          labs(title = 'Time: {frame_time}') +
          transition_time(as.POSIXct(departure_time)) +
          shadow_wake(wake_length = 0.015, alpha = FALSE) +
          ease_aes('linear') +
          theme_map()
          


# save gif
anim_save(animation = anim, "./tests_rafa/gif_spo_fps22_speed_allalpha0-1.gif", fps = 22)


# animate(anim, duration = 8, fps = 20, width = 400, height = 400, renderer = gifski_renderer()) %>%
# anim_save("./tests_rafa/gif.gif")


beepr::beep()






###### .gif tile ------------------

anim <- ggplot() +
  geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
  scale_fill_identity() +
  
  geom_sf(data=st_transform(shapes_sf, 3857), color='gray90', size=0.01) +
  geom_sf(data = st_transform(gps_sf, 3857), aes(colour = speed), size=1.5, alpha = 0.6, show.legend = FALSE) +
  scale_colour_viridis() +
  
  # gganimate specificatons
  labs(title = 'Time: {frame_time}') +
  transition_time(as.POSIXct(departure_time)) +
  shadow_wake(wake_length = 0.015, alpha = FALSE) +
  ease_aes('linear') +
  theme_map()



# save gif
anim_save(animation = anim, "./tests_rafa/gif_spo_fps22_speedtile.gif", fps = 22)


# animate(anim, duration = 8, fps = 20, width = 400, height = 400, renderer = gifski_renderer()) %>%
# anim_save("./tests_rafa/gif.gif")


beepr::beep()














  
############################ LINHA
  # https://github.com/thomasp85/gganimate/issues/226
  # 
  # https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
     

muni_sp <- geobr::read_municipality(code_muni = 'SP')
muni_sp <- subset(muni_sp, code_muni== 3550308)


a <- subset(dt, trip_id %in% unique(dt$trip_id)[1])

animation_r <- 
ggplot(data=a[order(trip_id, id)], aes(x=shape_pt_lon, y=shape_pt_lat, group=as.factor(trip_id)),  ) +
#    geom_sf(data=muni_sp, fill='gray20') +
    geom_path(alpha = 0.3, color = '#03f4f4', show.legend = FALSE) +
    geom_point(alpha = 0.3, color = '#03f4f4', show.legend = FALSE) +
    # scale_color_viridis_d() +
    coord_sf() +
    transition_reveal(as.POSIXct(departure_time)+7200) +
    ease_aes('linear') +
    theme_map() + 
    theme(plot.background = element_rect(fill = "gray20")) +
    coord_sf(ylim = c(-23.8, -23.3))



# 
# 
# anim_reveal <- 
#   ggplot() +
#   geom_sf(data = gps_sf, aes(colour = speed), size=1.5, alpha = 0.6, show.legend = FALSE) +
#   scale_colour_viridis() +
#   
#   # gganimate specificatons
#   labs(title = 'Time: {frame_time}') +
#   transition_reveal(as.POSIXct(departure_time)) +
#   ease_aes('linear') +
#   theme_map()
# 
# 
# 
# # save gif
# anim_save(animation = anim_reveal, "./tests_rafa/gif_spo_reveal.gif", fps = 22)



## save
# animate(animation_r, duration = 5, fps = 20, width = 400, height = 400, renderer = gifski_renderer())
# anim_save("./tests_rafa/gif_spo_path2.gif")

# Save
 anim_save(animation = animation_r, "./tests_rafa/gif_path.gif", fps = 20)

 