# gtfs2gps

### Converting public transport data from GTFS format to GPS-like records

**gtfs2gps** is an R package that converts public transportation data in GTFS format to GPS-like records in a `data.frame`/`data.table`, which can then be used in various applications such as running transport simulations or scenario analyses. 

The core function of the package takes a `GTFS.zip` file and interpolates the space-time position of each vehicle in each trip considering the network distance and average speed between stops, generating a `data.table` where each row represents the timestamp of each vehicle at a given spatial resolution. The package also has some functions to subset GTFS data in time and space and to convert both representations to [simple feature](https://CRAN.R-project.org/package=sf) format.

### Installation

Please install **gtfs2gps** package from GitHub, making sure you have the
latest version of the other packages it requires:

``` r
devtools::install_github("ipeaGIT/gtfs2gps")
library(gtfs2gps)
```

### Vignette

Please see our vignette:

* [gtfs2gps: Converting GTFS data to GPS format](https://github.com/ipeaGIT/gtfs2gps/blob/master/vignettes/intro_to_gtfs2gps.md)

### Credits

You can cite this package as:

* Pereira, R.H.M.; Andrade, P.R.; Bazzo, J. (2019) gtfs2gps: Converting GTFS data to GPS format. GitHub repository - https://github.com/ipeaGIT/gtfs2gps.
