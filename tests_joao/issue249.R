
# check adjust_speed----
gps_data_raw <- read_gtfs(system.file("extdata/fortaleza.zip", package="gtfs2gps")) %>%
  filter_week_days() %>% filter_single_trip() %>% gtfs2gps()

gps_fix <- adjust_speed(gps_data_raw)
gps_fix$old_speed <- gps_data_raw$speed
gps_fix
gps_fix
gps_fix1 <- adjust_speed1(gps_data_raw)
gps_fix1$old_speed <- gps_data_raw$speed
gps_fix1
gps_fix1

[1] "U804-T01V01B01-I" "U806-T01V01B01-I" "U810-T01V01B01-I" "U814-T01V01B01-I" "U815-T01V01B01-I"
[6] "U816-T01V01B01-I" "U820-T01V01B01-I" "U825-T01V01B01-I" "U832-T01V01B01-I" "U833-T01V01B01-I"
[11] "U836-T01V01B01-I" "U841-T01V01B01-I"
gps_rbind <- rbind( gps_fix[trip_id == "U825-T01V01B01-I",][,obs := "Old Method"],
                    gps_fix1[trip_id == "U825-T01V01B01-I",][,obs := "New Method"])

gps_rbind[,bus_stop := data.table::fifelse(!is.na(stop_sequence) 
                                           & round(as.numeric(dist)) == 0,
                                           TRUE,
                                           FALSE)]
gps_rbind[is.na(old_speed),speed_changed := "Sim"]
gps_rbind[!is.na(old_speed), speed_changed := fifelse(
  round(as.numeric(speed),1) != round(as.numeric(old_speed),1),"Sim","Nao")]
gps_rbind[speed_changed == "Sim",c("speed","old_speed","speed_changed")]

ggplot(gps_rbind)+
  geom_point( aes(x = cumdist,y = speed, group = obs),color = "black")+
  geom_line( aes(x = cumdist,y = speed, group = obs),color = "black")+
  geom_point(data = gps_rbind[bus_stop == TRUE,]
             , aes(x = cumdist,y = speed),color = "red")+
  geom_point(data = gps_rbind[speed_changed == "Sim",]
             , aes(x = cumdist,y = speed),color = "blue")+
  facet_wrap(~obs,nrow = 2)+
  theme(legend.position =  c(.95, .95))
