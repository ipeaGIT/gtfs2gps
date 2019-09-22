# gtfs2vein

### Converting public transport data from GTFS format to GPS-like records

For some GTFS feeds, like the one of Porto Alegre, there is only information about the departure/arrival time of trips at the first and the last stop of each trip*. When we convert the GTFS into GPS-like data frame, these are the data points we have recorded in the GTFS feed:

|trip_id|stop_id|departure_time|lat|long|stop_sequence|
|-----|-----|-----|-----|-----|-----|
| T1-2@1#600 | 1511 | 06:00:00 | -30.03729 | -51.23637|1|
| T1-2@1#600 | 1563 | ? | -30.03742 | -51.2316|2|
| T1-2@1#600 | 1566 | ? | -30.03845 | -51.23022|3|
| ... | ... | ... | ... | ...|...|
| T1-2@1#600 | 5503 | 06:53:00 | -30.01022 | -51.1456|65|

In this case, we cannot estimate the vehicle speed between each pair of stops. We can, however, easily estimate the average speed of the average speed of the vehicle over the entire trip. To do this, we need the length of the travelled distance of the vehicle in that trip.

```
# get total duration of trip
  stoptimes[, departure_time := as.POSIXct(departure_time, format="%H:%M:%OS")]

  trip_duration <- stoptimes[trip_id== "T1-2@1#600", 
                         departure_time[1L] - departure_time[.N] ]

# Convert all shapes into sf object
  shapes_sf <- shapes[
    , {
      geometry <- sf::st_linestring(x = matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2)) # add crs(!)
      geometry <- sf::st_sfc(geometry)
      geometry <- sf::st_sf(geometry = geometry)
    }
    , by = shape_id
    ]

  shapes_sf <- sf::st_as_sf(shapes_sf)

# length of the trip
  trip_dist <- subset(shapes_sf, shape_id == "T1-2") %>% st_length()

avg_speed = trip_dist / trip_duration

```

With these information at hand, we can choose the spatial resolution we want in our data GPS-like data. Let's say we want a timestamp every 20 seconds. In this case, this is the structure of the dataframe we want to arrive at. What we need now is to interpolate in space what is the location of the vehicle given that we know the its average speed `avg_speed` and trajectory `shapes_sf`


|trip_id|stop_id|departure_time|lat|long|stop_sequence|
|-----|-----|-----|-----|-----|-----|
| T1-2@1#600 | 1511 | 06:00:00 | -30.03729 | -51.23637|1|
| T1-2@1#600 | moving | 06:00:20 | ? | ?|1.1|
| T1-2@1#600 | moving | 06:00:40 | ? | ?|1.2|
| T1-2@1#600 | moving | 06:01:00 | ? | ?|1.3|
| ... | ... | ... | ... | ...|...|
| T1-2@1#600 | 1563 | ? | -30.03742 | -51.2316|2|


NOTE * Other GTFS feeds are far more detailed, bringing information for arrival/departure time for every trip at every stop. In these cases we will have to adopt a slightly different approach so we can use `speed` information between every pair of consecutive stops.
