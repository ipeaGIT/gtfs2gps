library(gtfs2gps)
library(magrittr)
library(ggplot2)
library(gganimate)
library(data.table)
library(ggthemes)
library(sf)
library(viridis)

gc(reset = T)


Sys.setenv(TZ= Sys.timezone() )


 
###### Prepare data  ------------------

# local GTFS.zip
gtfs_zip <- system.file("extdata/saopaulo.zip", package="gtfs2gps" )

# read gtfs
gtfs_dt <- gtfs2gps::read_gtfs(gtfs_zip)

# read gtfs
gtfs_dt <- gtfs2gps::filter_week_days(gtfs_dt)


# # subset time interval
# spo_gtfs_f <- gtfs2gps::filter_day_period(spo_gtfs, period_start = "07:00:", period_end = "08:50")
  
# get network as sf object
shapes_sf <- gtfs_shapes_as_sf(gtfs_dt)

# Convert GTFS data into a data.table with GPS-like records
gps_dt <- gtfs2gps(gtfs_dt, spatial_resolution = 15, progress = T, parallel = T )

# subset time interval
gps_dt <- gps_dt[ between(departure_time, as.ITime("07:00:"), as.ITime("07:30"))]






###### Static plot ------------------

# static plot: routes

ggplot() + 
  geom_sf(data=shapes_sf, color='red') +
  coord_sf() 




###### .gif plot ------------------

anim <- ggplot() +
          geom_sf(data=shapes_sf, color='gray90', size=0.01) +
          geom_point(data = gps_dt, aes(x = shape_pt_lon, y=shape_pt_lat, colour = speed), size=1.5, alpha = 0.6, show.legend = FALSE) +
          scale_colour_viridis() +
          
          # gganimate specificatons
          labs(title = 'Time: {frame_time}') +
          transition_time(as.POSIXct(departure_time)) +
          shadow_wake(wake_length = 0.015, alpha = FALSE) +
          ease_aes('linear') +
          theme_map()
          


# save gif
anim_save(animation = anim, "./tests_rafa/gif_spo_fps22_speed.gif", fps = 22)


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



## save
# animate(animation_r, duration = 5, fps = 20, width = 400, height = 400, renderer = gifski_renderer())
# anim_save("./tests_rafa/gif_spo_path2.gif")

# Save
 anim_save(animation = animation_r, "./tests_rafa/gif_path.gif", fps = 20)

 