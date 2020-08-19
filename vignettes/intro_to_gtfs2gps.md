gtfs2gps: Converting GTFS data to GPS-like format
================
Rafael H. M. Pereira, Pedro R. Andrade, Joao Bazzo

06 July 2020

# Introduction

Package `gtfs2gps` allows users to convert public transport GTFS data
into a single `data.table` format with GPS-like records, which can then
be used in various applications such as running transport simulations or
scenario analyses. Before using the package, just install it from
GitHub.

``` r
install.packages("gtfs2gps")
```

# Loading data

After loading the package, GTFS data can be read into R by using
`read_gtfs()`. This function gets a zipped GTFS file and returns a list
of `data.table` objects. The returning list contains the data of each
GTFS file indexed according to their file names without extension.

``` r
library("data.table")
library("gtfs2gps")
sao <- read_gtfs(system.file("extdata/saopaulo.zip", package ="gtfs2gps"))
names(sao)
```

    ## [1] "agency"      "routes"      "stops"       "stop_times"  "shapes"     
    ## [6] "trips"       "calendar"    "frequencies"

``` r
sao$trips
```

    ##      route_id service_id   trip_id    trip_headsign direction_id shape_id
    ##   1:  121G-10        USD 121G-10-0   Metrô Tucuruvi            0    52421
    ##   2:  148L-10        USD 148L-10-0             Lapa            0    52857
    ##   3:  148L-10        USD 148L-10-1  Cohab Antártica            1    52858
    ##   4:  1720-10        USD 1720-10-0       Cantareira            0    54502
    ##   5:  1720-10        USD 1720-10-1       Jd. Guancã            1    54503
    ##  ---                                                                     
    ## 229:  N732-11        USD N732-11-0 Term. Jd. Jacira            0    51990
    ## 230:  N739-11        USD N739-11-0    Jd. Universal            0    51954
    ## 231:  N740-11        USD N740-11-0      Jd. Riviera            0    51939
    ## 232:  N838-11        USD N838-11-0  Cptm Leopoldina            0    52072
    ## 233:  N840-11        USD N840-11-0     Sta. Cecília            0    52135

Note that not all GTFS files are loaded into R. This function only loads
the necessary data to spatially and temporally handle trips and stops,
which are: “shapes.txt”, “stop\_times.txt”, “stops.txt”, “trips.txt”,
“agency.txt”, “calendar.txt”, “routes.txt”, and “frequencies.txt”,
with this last four being optional. If a given GTFS zipped file does not
contain all of these required files then `read_gtfs()` will stop with an
error.

# Subsetting GTFS Data

GTFS data sets can be fairly large for complex public transport networks
and, in some cases, users might want to focus on specific transport
services at week days/weekends, or on specific trips or routes. The
package brings some functions to filter GTFS.zip and speed up the data
processing.

  - **filter\_by\_shape\_id():** Filter shapes using given shape ids.
  - **filter\_by\_agency\_id():** Filter routes using given agency ids.
  - **filter\_valid\_stop\_times():** Return only stop times that have
    geospatial locations.
  - **filter\_week\_days():** Remove weekend trips.
  - **filter\_single\_trip():** Return only one trip per shape\_id.
  - **filter\_by\_route\_type():** Filter by transport mode.
  - **filter\_by\_route\_id():** Filter routes and trips by route id.
  - **filter\_day\_period():** Filter according to a time interval.
  - **remove\_invalid():** Remove all inconsistent data, checking all
    relations.

These functions subset all the relevant GTFS files in order to remove
all the unnecessary rows, keeping the data consistent. The returning
values of the four functions is a list of `data.table` objects, in the
same way of the input data. For example, in the code below we filter
only shape ids between 53000 and 53020.

``` r
library(magrittr)
object.size(sao) %>% format(units = "Kb")
```

    ## [1] "6227.2 Kb"

``` r
sao_small <- gtfs2gps::filter_by_shape_id(sao, c(51338, 51956, 51657))
object.size(sao_small) %>% format(units = "Kb")
```

    ## [1] "105.8 Kb"

We can then easily convert the data to simple feature format and plot
them.

``` r
sao_small_shapes_sf <- gtfs2gps::gtfs_shapes_as_sf(sao_small)
sao_small_stops_sf <- gtfs2gps::gtfs_stops_as_sf(sao_small)
plot(sf::st_geometry(sao_small_shapes_sf))
plot(sf::st_geometry(sao_small_stops_sf), pch = 20, col = "red", add = TRUE)
box()
```

![](C:/Users/pedro/AppData/Local/Temp/RtmpcZbTEQ/preview-5a1870246f4f.dir/intro_to_gtfs2gps_files/figure-gfm/sao_small_shapes_sf-1.png)<!-- -->

After subsetting the data, it is also possible to save it as a new GTFS
file using `write_gtfs()`, as shown below.

``` r
write_gtfs(sao_small, "sao_small.zip")
```

# Converting to GPS-like format

To convert GTFS to GPS-like format, use `gtfs2gps()`. This is the core
function of the package. It takes a GTFS zipped file as an input and
returns a `data.table` where each row represents a ‘GPS-like’ data point
for every trip in the GTFS file. In summary, this function interpolates
the space-time position of each vehicle in each trip considering the
network distance and average speed between stops. The function samples
the timestamp of each vehicle every \(15m\) by default, but the user can
set a different value in the `spatial_resolution` argument. See the
example below.

``` r
  sao_gps <- gtfs2gps("sao_small.zip", progress = FALSE, parallel = FALSE, spatial_resolution = 50)
  head(sao_gps)
```

    ##    id shape_id   trip_id trip_number route_type shape_pt_lon shape_pt_lat
    ## 1:  1    51338 5010-10-0           1          3    -46.63120    -23.66268
    ## 2:  2    51338 5010-10-0           1          3    -46.63117    -23.66273
    ## 3:  3    51338 5010-10-0           1          3    -46.63108    -23.66288
    ## 4:  4    51338 5010-10-0           1          3    -46.63095    -23.66316
    ## 5:  5    51338 5010-10-0           1          3    -46.63082    -23.66345
    ## 6:  6    51338 5010-10-0           1          3    -46.63111    -23.66364
    ##    departure_time stop_id stop_sequence      dist    cumdist    cumtime   speed
    ## 1:       04:00:01 3703053             1  7.230445   7.230445  0.9788103 26.5931
    ## 2:       04:00:03    <NA>            NA 18.369274  25.599720  3.4655221 26.5931
    ## 3:       04:00:08    <NA>            NA 34.505965  60.105685  8.1367134 26.5931
    ## 4:       04:00:13    <NA>            NA 34.505965  94.611650 12.8079046 26.5931
    ## 5:       04:00:18    <NA>            NA 36.478776 131.090426 17.7461620 26.5931
    ## 6:       04:00:23    <NA>            NA 36.478776 167.569201 22.6844194 26.5931

The following figure maps the first 100 data points of the sample data
we processed. They can be converted to `simple feature` points or
linestring.

``` r
  sao_gps60 <- sao_gps[1:100, ]
  
  # points
  sao_gps60_sfpoints <- gps_as_sfpoints(sao_gps60)
  
  # linestring
  sao_gps60_sflinestring <- gps_as_sflinestring(sao_gps60)

  # plot
  plot(sf::st_geometry(sao_gps60_sfpoints), pch = 20)
  plot(sf::st_geometry(sao_gps60_sflinestring), col = "blue", add = TRUE)
  box()
```

![](C:/Users/pedro/AppData/Local/Temp/RtmpcZbTEQ/preview-5a1870246f4f.dir/intro_to_gtfs2gps_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

The function `gtfs2gps()` automatically recognizes whether the GTFS data
brings detailed `stop_times.txt` information or whether it is a
`frequency.txt` GTFS file. A sample data of a GTFS with detailed
`stop_times.txt` cab be found below:

``` r
poa <- system.file("extdata/poa.zip", package ="gtfs2gps")

poa_gps <- gtfs2gps(poa, progress = FALSE, parallel = FALSE, spatial_resolution = 50)

poa_gps_sflinestrig <- gps_as_sfpoints(poa_gps)

plot(sf::st_geometry(poa_gps_sflinestrig[1:200,]))

box()
```

![](C:/Users/pedro/AppData/Local/Temp/RtmpcZbTEQ/preview-5a1870246f4f.dir/intro_to_gtfs2gps_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Methodological note

For a given trip, the function `gtfs2gps` calculates the average speed
between each pair of consecutive stops — given by the ratio between
cumulative network distance `S` and departure time `t` for a consecutive
pair of valid stop\_ids (`i`),

\[Large Speed_i = \frac{S_{i+1}-S_i}{t_{i+1}-t_i}\]

Since the beginning of each trip usually starts before the first
stop\_id, the mean speed cannot be calculated as shown in the previous
equation because information on `i` period does not exist. In this case,
the function consider the mean speed for the whole trip. It also happens
after the last valid stop\_id (`N`) of the trips, where info on `i + 1`
also does not exist.

![](https://github.com/ipeaGIT/gtfs2gps/blob/master/man/figures/speed.PNG)<!-- -->

# Final remarks

If you have any suggestions or want to report an error, please visit the
GitHub page of the package [here](https://github.com/ipeaGIT/gtfs2gps).
