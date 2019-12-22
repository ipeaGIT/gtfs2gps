install.packages('moveVis')
install.packages('move')

class(poa60$departure_time)
poa60$time2 <- as.POSIXct(poa60$departure_time)+7200


poa60_2 <- poa60[!duplicated(poa60),]

x <- df2move(data.frame(poa60), y='shape_pt_lat', x='shape_pt_lon', track_id = 'trip_id', proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", time= 'time2' )


> Error in validityMethod(as(object, superClass)) : 
>   The dataset includes double timestamps first one:2019-12-21 07:44:59)

dt <- poa60[ departure_time == as.ITime("07:44:59") ]


x <- df2move(data.frame(poa60), y='shape_pt_lat', x='shape_pt_lon', track_id = 'trip_id', proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", time= 'time2' , removeDuplicatedTimestamps=TRUE)



library(moveVis)
library(move)

data("move_data", package = "moveVis") # move class object
# if your tracks are present as data.frames, see df2move() for conversion

head(move_data)
class(move_data)


# align move_data to a uniform time scale
m <- align_move(move_data, res = 240, digit = 0, unit = "secs")

# create spatial frames with a OpenStreetMap watercolour map
frames <- frames_spatial(m, path_colours = c("red", "green", "blue"),
                         map_service = "osm", map_type = "watercolor", alpha = 0.5) %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% # add some customizations, such as axis labels
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(m, type = "label") %>% 
  add_progress()

frames[[100]] # preview one of the frames, e.g. the 100th frame

# animate frames
animate_frames(frames, out_file = "/full/path/to/moveVis.gif")



