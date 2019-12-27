library(gtfs2gps)
library(magrittr)
library(ggplot2)
library(gganimate)
library(data.table)
library(ggthemes)
library(sf)
gc(reset = T)

 
# poa <- system.file("extdata/poa.zip", package="gtfs2gps" )
# system.time(poa150 <- gtfs2gps(poa, progress = T, spatial_resolution = 150))
# system.time(poa300 <- gtfs2gps(poa, progress = T, spatial_resolution = 300))
# system.time(poa600 <- gtfs2gps(poa, progress = T, spatial_resolution = 5000))

# Generate GPS-like data

spo <- system.file("extdata/saopaulo.zip", package="gtfs2gps" )
system.time(spo_gps60 <- gtfs2gps(spo, spatial_resolution = 100, progress = T ))
table(spo_gps$trip_id)

gtfs_for <- 'L:\\Proj_acess_oport\\data-raw\\gtfs\\for\\gtfs_for_etufor_2019-10.zip'
system.time(gtfs_for <- gtfs2gps(gtfs_for, spatial_resolution = 100, progress = T )) # 67.85 minutos com 19 cores


# # get static network
# # spo_sf <- gtfs_shapes_as_sf(spo) ?????????????
# network <- spo_gps[, .(trip_id=trip_id[1L], shape_pt_lon=shape_pt_lon[1L], shape_pt_lat =shape_pt_lat[1L] ), by= .(shape_id, id) ]
# network_sf <- gps_as_sf(network)
# ggplot() + geom_sf(data=network_sf[1,], aes(color=shape_id) )


# dt <- subset(spo_gps, trip_id %in% c('176-1@1#1028' , 'A141-1@1#1750', 'R10-2@1#1010'))
# dt <- subset(spo_gps, trip_id %in% unique(spo_gps$trip_id)[1:3] )
# dt <- subset(spo_gps, trip_id %in% c('176-1@1#1028' , 'A141-1@1#1750', 'R10-2@1#1010'))
dt <- spo_gps60[ between(departure_time, lower = as.ITime("07:00:00"), upper = as.ITime("07:50:00")) ]




dt_sf <- gps_as_sf(dt)




p <- ggplot(data = dt, 
      aes(x = shape_pt_lon, y=shape_pt_lat, colour = trip_id)) +
      geom_point(show.legend = FALSE, alpha = 0.7) +
      scale_color_viridis_d()

p2 <- ggplot(data = dt[order(trip_id, id)], aes(x = shape_pt_lon, y=shape_pt_lat, colour = trip_id)) +
        geom_path(aes(group=trip_id), show.legend = FALSE, alpha = 0.7) +
        scale_color_viridis_d()

p2










############################ PONTO

anim <- ggplot() +
#          geom_sf(data=network_sf, color='gray70', size=0.01) +
          geom_point(data = dt, aes(x = shape_pt_lon, y=shape_pt_lat, colour = trip_id), size=2, show.legend = FALSE, alpha = 0.6) +
          scale_color_viridis_d() +
          
          # Here comes the gganimate specific bits
          labs(title = 'Time: {frame_time}') +
          transition_time(as.POSIXct(departure_time)+7200) +
          shadow_wake(wake_length = 0.015, alpha = FALSE) +
          ease_aes('linear') +
          theme_map()
          



animate(anim, duration = 8, fps = 20, width = 400, height = 400, renderer = gifski_renderer()) %>%
anim_save("./tests_rafa/gif_spo_ponto_0-015_2.gif")

anim_save(animation =anim, "./tests_rafa/gif_spo_ponto_0-015_4.gif")
beepr::beep()

  
############################ LINHA
  # https://github.com/thomasp85/gganimate/issues/226
  # 
  # https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
     

muni_sp <- geobr::read_municipality(code_muni = 'SP')
muni_sp <- subset(muni_sp, code_muni== 3550308)


animation_r <- 

ggplot() +
    geom_sf(data=muni_sp, fill='gray20') +
    geom_path(data=dt[order(trip_id, id)], aes(x=shape_pt_lon, y=shape_pt_lat, group=as.factor(shape_id)), show.legend = FALSE, alpha = 0.3, color = '#03f4f4' ) +
    # scale_color_viridis_d() +
    transition_reveal(as.POSIXct(departure_time)+3600) +
    ease_aes('linear') +
    theme_map() + 
    theme(plot.background = element_rect(fill = "gray20")) +
    coord_sf(ylim = c(-23.8, -23.3))


# salve
animate(animation_r, duration = 5, fps = 20, width = 400, height = 400, renderer = gifski_renderer())
anim_save("./tests_rafa/gif_spo_path2.gif")

## alternative save
# anim_save(animation = animation_r, "./tests_rafa/gif_spo_path.gif")
# beepr::beep()

  
  
  