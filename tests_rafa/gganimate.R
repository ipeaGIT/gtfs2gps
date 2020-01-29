library(gtfs2gps)
library(magrittr)
library(ggplot2)
library(gganimate)
library(data.table)
library(ggthemes)
library(sf)
gc(reset = T)

 
###### Prepare data  ------------------

# local GTFS.zip
spo_zip <- system.file("extdata/saopaulo.zip", package="gtfs2gps" )

 # spo_zip <- system.file("extdata/poa.zip", package="gtfs2gps")

# read gtfs
spo_gtfs <- gtfs2gps::read_gtfs(spo_zip)

# subset time interval
spo_gtfs_f <- gtfs2gps::filter_day_period(spo_gtfs, period_start = "07:00:", period_end = "08:50")
  

# Convert GTFS data into a data.table with GPS-like records
spo_gps <- gtfs2gps(spo_gtfs_f, spatial_resolution = 15, progress = T, cores = 1 )


spo_gtfs$trips[ trip_id =="8700-21-0"]


# get static network as sf object
spo_sf <- read_gtfs(spo_zip) %>% gtfs_shapes_as_sf()





###### Static plot ------------------

# plot static shape

ggplot() + 
  geom_sf(data=spo_sf, color='red') +
  coord_sf() 




###### .gif plot ------------------

anim <- ggplot() +
          geom_sf(data=spo_sf, color='gray90', size=0.01) +
          geom_point(data = dt, aes(x = shape_pt_lon, y=shape_pt_lat, colour = trip_id), size=1.5, alpha = 0.6, show.legend = FALSE) +
          scale_color_viridis_d() +
          
          # gganimate specificatons
          labs(title = 'Time: {frame_time}') +
          transition_time(as.POSIXct(departure_time)+7200) +
          shadow_wake(wake_length = 0.015, alpha = FALSE) +
          ease_aes('linear') +
          theme_map()
          


# save gif
anim_save(animation = anim, "./tests_rafa/gif_spo_point_22fps.gif", fps = 22)


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

 