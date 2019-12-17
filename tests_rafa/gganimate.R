library(gtfs2gps)
library(magrittr)
library(ggplot2)
library(gganimate)
library(data.table)
library(ggthemes)


# Generate GPS-like data
poa <- system.file("extdata/poa.zip", package="gtfs2gps")
poa_gps <- gtfs2gps(poa, progress = FALSE)
table(poa_gps$trip_id)

# get static network
# poa_sf <- gtfs_shapes_as_sf(poa) ?????????????
network <- poa_gps[, .(trip_id=trip_id[1L], shape_pt_lon=shape_pt_lon[1L], shape_pt_lat =shape_pt_lat[1L] ), by= .(shape_id, id) ]
network_sf <- gps_as_sf(network)
ggplot() + geom_sf(data=network_sf, aes(color=shape_id) )


# dt <- subset(poa_gps, trip_id %in% c('176-1@1#1028' , 'A141-1@1#1750', 'R10-2@1#1010'))
# dt <- subset(poa_gps, trip_id %in% unique(poa_gps$trip_id)[1:10] )
# dt <- subset(poa_gps, trip_id %in% c('176-1@1#1028' , 'A141-1@1#1750', 'R10-2@1#1010'))
dt <- poa_gps[ between(departure_time, lower = as.ITime("07:00:00"), upper = as.ITime("07:30:00")) ]




dt_sf <- gps_as_sf(dt)




p <- ggplot(data = dt, 
      aes(x = shape_pt_lon, y=shape_pt_lat, colour = trip_id)) +
      geom_point(show.legend = FALSE, alpha = 0.7) +
      scale_color_viridis_d()

p














############################ PONTO

anim <- ggplot() +
          geom_sf(data=network_sf, color='gray70', size=0.01) +
          geom_point(data = dt, aes(x = shape_pt_lon, y=shape_pt_lat, colour = trip_id), show.legend = FALSE, alpha = 0.7) +
          scale_color_viridis_d() +
          # Here comes the gganimate specific bits
          labs(title = 'Time: {frame_time}') +
          transition_time(as.POSIXct(departure_time)) +
          shadow_wake(wake_length = 0.015, alpha = FALSE) +
          ease_aes('linear') +
          theme_map()
          


anim_save(animation =anim, "a_0.015.gif")
beepr::beep()
  
  
############################ LINHA
https://github.com/thomasp85/gganimate/issues/226

https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
  
  
animation_r <- 

ggplot() +
    geom_path(data=dt, aes(color = trip_id,  x=shape_pt_lon, y=shape_pt_lat), show.legend = FALSE, alpha = 0.7 ) +
    # geom_point(show.legend = FALSE, alpha = 0.7) +
    scale_color_viridis_d() +
    # Here comes the gganimate specific bits
    labs(title = 'Time: {frame_time}') +
    transition_reveal(as.POSIXct(departure_time)) +
    ease_aes('linear') +
    theme_map()

  
anim_save(animation = animation_r, "r_0.015.gif")
beepr::beep()

  
  
  