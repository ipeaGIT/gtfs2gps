# log history of gtfs2gps package development


-------------------------------------------------------
# gtfs2gps dev v1.4-0

**Major changes**
* Allows filtering a GTFS feed by weekday and accounts for `calendar_dates.txt` info. Closes #103, #124 and #141
* New sample GTFS.zip data for Berlin

**Minor changes**
* Fixed a bug in filter_day_period. Closes #96
* Fix the treatment of midnight trips. Closes #43




-------------------------------------------------------
# gtfs2gps v1.3-2

**Major changes**
* New function to merge GTFS feeds. Closes #34
* New pkgdown website. https://ipeagit.github.io/gtfs2gps/ . Closes #146
* Distance of 1st point of GPS trip now start with distance zero. Closes #136

**Minor changes**
* changes parallel execution to conform new `future` standards. closes #55
* Improve time filter in `filter_day_period`. Closes #89 and 147
* Improved documentation of  `spatial_resolution` parameterCloses #116


-------------------------------------------------------
# gtfs2gps v1.3-0

**Major changes**
* Use progress bar from progressr. Closes #142


-------------------------------------------------------
# gtfs2gps v1.2-1


-------------------------------------------------------
# gtfs2gps v1.2-0


-------------------------------------------------------
# gtfs2gps v1.1-0


-------------------------------------------------------
# gtfs2gps v1.0-7


-------------------------------------------------------
# gtfs2gps v1.0-5

* Launch of **gtfs2gps** v1.0-5 on [CRAN](https://cran.r-project.org/web/packages/gtfs2gps/index.html) on 2020-03-16