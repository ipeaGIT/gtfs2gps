# gtfs2gps

### Converting public transport data from GTFS format to GPS-like records

Package `gtfs2gps` converts public transport GTFS data to GPS-like records using [`data.table`](https://cran.r-project.org/web/packages/data.table/index.html) format. It also has some functions to subset GTFS data in time and space and to convert both representations to [simple feature](https://cran.r-project.org/web/packages/sf/index.html) format.

### Installation

Please install gtfs2gps package from GitHub, making sure you have the
latest version of the other packages it requires:

``` r
devtools::install_github("ipeaGIT/gtfs2gps")
library(gtfs2gps)
```

### Vignette

Please see our vignette:

* [gtfs2gps: Converting GTFS data to GPS format](https://github.com/ipeaGIT/gtfs2gps/blob/master/vignettes/gtfs2gps.pdf)

### Credits

You can cite this package as:

* Pereira, R.H.M.; Andrade, P.R.; Bazzo, J. (2019) gtfs2gps: Converting GTFS data to GPS format. GitHub repository - https://github.com/ipeaGIT/gtfs2gps.
