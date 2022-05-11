# gtfs2gps: Converting public transport data from GTFS format to GPS-like records <img align="right" src="man/figures/logo.png" alt="logo" width="180"> 

[![CRAN/METACRAN Version](https://www.r-pkg.org/badges/version/gtfs2gps)](https://CRAN.R-project.org/package=gtfs2gps).[![CRAN/METACRAN Total downloads](http://cranlogs.r-pkg.org/badges/grand-total/gtfs2gps?color=blue)](https://CRAN.R-project.org/package=gtfs2gps)
[![R-CMD-check](https://github.com/ipeaGIT/gtfs2gps/workflows/R-CMD-check/badge.svg)](https://github.com/ipeaGIT/gtfs2gps/actions)
[![Codecov test coverage](https://codecov.io/gh/ipeaGIT/gtfs2gps/branch/master/graph/badge.svg)](https://codecov.io/gh/ipeaGIT/gtfs2gps?branch=master)
[![DOI](https://zenodo.org/badge/203697230.svg)](https://zenodo.org/badge/latestdoi/203697230)

**gtfs2gps** is an R package that converts public transportation data in GTFS format to GPS-like records in a `data.frame`/`data.table`, which can then be used in various applications such as running transport simulations or scenario analyses. 

The core function of the package takes a `GTFS.zip` file and interpolates the space-time position of each vehicle in each trip considering the network distance and average speed between stops. The output is a `data.table` where each row represents the timestamp of each vehicle at a given spatial resolution. The package also has some functions to subset GTFS data in time and space and to convert both representations to [simple feature](https://CRAN.R-project.org/package=sf) format. More information about the methods used in the package can be found in this [preprint](https://osf.io/preprints/socarxiv/qydr6/).

### Installation

Please install **gtfs2gps** package from CRAN to get the stable version.

``` r
install.packages("gtfs2gps")
library(gtfs2gps)
```

### Vignette

Please see our vignette:

* [gtfs2gps: Converting GTFS data to GPS format](https://ipeagit.github.io/gtfs2gps/articles/intro_to_gtfs2gps.html)



-----

### Credits <img align="right" src="man/figures/ipea_logo.png" alt="ipea" width="300">

The **gtfs2gps** package is developed by a team at the Institute for Applied Economic Research (Ipea) with collaboration from the National Institute for Space Research (INPE), both from Brazil. You can cite this package as:

* Pereira, R.H.M.; Andrade, P.R.; Bazzo, J. (2019) gtfs2gps: Converting GTFS data to GPS format. Zenodo https://zenodo.org/badge/latestdoi/203697230.



