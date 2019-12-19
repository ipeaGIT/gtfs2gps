gtfs2gps: Converting GTFS data to GPS-like format
================
Rafael H. M. Pereira, Pedro R. Andrade, Joao Bazzo

15 December 2019

# Introduction

Package `gtfs2gps` allows users to convert public transport GTFS data
into a single `data.table` format with GPS-like records, which can then
be used in various applications such as running transport simulations or
scenario analyses. Before using the package, just install it from
GitHub.

``` r
devtools::install_github("ipeaGIT/gtfs2gps")
```

# Loading data

After loading the package, GTFS data can be read into R by using
`read_gtfs()`. This function gets a zipped GTFS file and returns a list
of `data.table` objects. The returning list contains the data of each
GTFS file indexed according to their file names without extension.

``` r
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
which are: - agency.txt - calendar.txt - routes.txt - shapes.txt -
stop\_times.txt - stops.txt - trips.txt - frequencies.txt (this last one
is optional).

If a given GTFS zipped file does not contain all of these required files
then `read_gtfs()` will stop with an error.

# Subsetting GTFS Data

GTFS data sets can be fairly large for complex public transport networks
and, in some cases, users might want to focus on specific transport
services at week days/weekends, or on specific trips or routes. The
package brings some functions to filter GTFS.zip and speed up the data
processing.

These functions subset all the relevant GTFS files in order to remove
all the unnecessary rows, keeping the data consistent. The returning
values of the four functions is a list of `data.table` objects, in the
same way of the input data. For example, in the code below we filter
only shape ids between 53000 and 53020.

``` r
library(magrittr)
object.size(sao) %>% format(units = "Kb")
```

    ## [1] "5419.4 Kb"

``` r
sao_small <- gtfs2gps::filter_by_shape_id(sao, c(51338, 51956, 51657))
object.size(sao_small) %>% format(units = "Kb")
```

    ## [1] "88.5 Kb"

We can then easily convert the data to simple feature format and plot
them.

``` r
sao_small_shapes_sf <- gtfs2gps::gtfs_shapes_as_sf(sao_small)
```

    ## Linking to GEOS 3.6.1, GDAL 2.2.3, PROJ 4.9.3

``` r
sao_small_stops_sf <- gtfs2gps::gtfs_stops_as_sf(sao_small)
plot(sf::st_geometry(sao_small_shapes_sf))
plot(sf::st_geometry(sao_small_stops_sf), pch = 20, col = "red", add = TRUE)
box()
```

![](untitled_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
![](https://github.com/ipeaGIT/gtfs2gps/tree/master/man/figures/sao_small_shapes_sf.jpg)

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
  sao_gps <- gtfs2gps("sao_small.zip", progress = FALSE, cores = 1, spatial_resolution = 15)
```

    ## Unzipping and reading GTFS.zip file

    ## converting shapes and stops to sf objects

    ## Processing the data

``` r
  head(sao_gps)
```

    ##      trip_id route_type id shape_pt_lon shape_pt_lat departure_time
    ## 1: 5010-10-0          3  1    -46.63120    -23.66268       04:00:01
    ## 2: 5010-10-0          3  2    -46.63117    -23.66273       04:00:02
    ## 3: 5010-10-0          3  3    -46.63113    -23.66281       04:00:04
    ## 4: 5010-10-0          3  4    -46.63108    -23.66288       04:00:05
    ## 5: 5010-10-0          3  5    -46.63103    -23.66299       04:00:07
    ## 6: 5010-10-0          3  6    -46.63098    -23.66311       04:00:09
    ##    stop_id stop_sequence      dist   cumdist    speed   cumtime shape_id
    ## 1: 3703053             1  7.230445  7.230445 26.11298 0.9968071    51338
    ## 2:      NA            NA  9.184637 16.415083 26.11298 2.2630239    51338
    ## 3:      NA            NA  9.184637 25.599720 26.11298 3.5292407    51338
    ## 4:      NA            NA 13.802386 39.402106 26.11298 5.4320717    51338
    ## 5:      NA            NA 13.802386 53.204492 26.11298 7.3349028    51338
    ## 6:      NA            NA 13.802386 67.006878 26.11298 9.2377339    51338

The following figure maps the first 100 data points of the sample data
we processed.

``` r
  shapes_sf <- gps_as_sf(sao_gps)
  sao_gps60 <- sao_gps[1:100, ]
  sao_gps60_sf <- gps_as_sf(sao_gps60)
  plot(sf::st_geometry(sao_gps60_sf), pch = 20)
  plot(sf::st_geometry(sao_small_shapes_sf), col = "blue", add = TRUE)
  box()
```

![](untitled_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
![](https://github.com/ipeaGIT/gtfs2gps/blob/master/man/figures/sao_gps60_sf.jpg)

The function `gtfs2gps()` automatically recognises whether the GTFS data
brings detailed `stop_times.txt` information or whether it is a
`frequency.txt` GTFS file. A sample data of a GTFS with detailed
`stop_times.txt` cab be found below:

``` r
poa <- system.file("extdata/poa.zip", package ="gtfs2gps")
poa_gps <- gtfs2gps(poa, progress = FALSE)
```

    ## Unzipping and reading GTFS.zip file

    ## converting shapes and stops to sf objects

    ## Processing the data

``` r
poa_gps_sf <- gps_as_sf(poa_gps)
poa_sf <- read_gtfs(poa) %>% gtfs_shapes_as_sf()
plot(sf::st_geometry(poa_gps_sf[1:200,]))
plot(sf::st_geometry(poa_sf), col = "blue", add = TRUE)
box()
```

![](untitled_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
![](https://github.com/ipeaGIT/gtfs2gps/blob/master/man/figures/poa.jpg)

# Methodological note

For a given trip, the function `gtfs2gps` calculates the average speed
between each pair of stops — given by the ratio between cumulative
network distance \(S\) and departure time \(t\) for a consecutive pair
of valid stop\_ids (\(i\)),

<img src="https://latex.codecogs.com/svg.latex?\Large&space;Speed_i=\frac{S_{i+1}-S_i}{t_{i+1}-t_i}" title="\Large Speed_i = \frac{S_{i+1}-S_i}{t_{i+1}-t_i}" />

Since the beginning of each trip usually starts before the first
stop\_id, the mean speed cannot be calculated as shown in the previous
equation because information on \(i\) period does not exist. In this
case, the function consider the mean speed for the whole trip. It also
happens after the last valid stop\_id (\(N\)) of the trips, where info
on \(i+1\) also does not exist.

![](https://github.com/ipeaGIT/gtfs2gps/tree/master/man/figures/speed.png)

# Final remarks

If you have any suggestions or want to report an error, please visit the
GitHub page of the package [here](https://github.com/ipeaGIT/gtfs2gps).
