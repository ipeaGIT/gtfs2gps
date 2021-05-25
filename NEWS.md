# log history of gtfs2gps package development

-------------------------------------------------------
# gtfs2gps dev v1.5 dev

**Major changes**
* Fixed the update of trip_number attribute. This affects the output of `gps_as_sflinestring()`. Closes #189.
* Imports the `gtfsio` package, used in the `read_gtfs()` and `write_gtfs()` functions. Closes #191.


**Minor changes**
* Function `filter_single_trip()` now also filters the `stop_times` table. Closes #195.

-------------------------------------------------------
# gtfs2gps dev v1.4-0

**Major changes**
* Allows filtering a GTFS feed by weekday and accounts for `calendar_dates.txt` info. Closes #103, #124 and #141
* New sample GTFS.zip data for Berlin
* New `compress` parameter in `gtfs2gps` function so that users can save the function outputs in compressed `.RDS` format.
* New function `remove_by_route_id()` to remove GTFS data by route ids. Closes #180
* Stops are now snapped to shapes using a simpler and more restrictive algorithm. This help identifies when a route is has problem with `stop_sequence` in the wrong order

**Minor changes**
* Fixed a bug in filter_day_period. Closes #96
* Fix the treatment of midnight trips. Closes #43
* Fix linebreak gps_as_sflinestring. Closes #171
* Warnings are now printed as messages
* Order stop sequence before converting to GPS.
* Additional verifications in gtfs2gps
* Added package Hex sticker Closes #75


-------------------------------------------------------
# gtfs2gps v1.3-2

**Major changes**
* New function to merge GTFS feeds. Closes #34
* New pkgdown website. https://ipeagit.github.io/gtfs2gps/ . Closes #146
* Distance of 1st point of GPS trip now start with distance zero. Closes #136

**Minor changes**
* changes parallel execution to conform new `future` standards. closes #55
* Improve time filter in `filter_day_period`. Closes #89 and 147
* Improved documentation of  `spatial_resolution` parameter. Closes #116

-------------------------------------------------------
# gtfs2gps v1.3-0

**Major changes**
* Use progress bar from progressr. Closes #142
* Handling units of measurement
* Speeding up some algorithms

-------------------------------------------------------
# gtfs2gps v1.2-1

**Minor changes**
* Allowing to read gtfs files without calendar/agency
* New verifications to create a better feedback for the user

-------------------------------------------------------
# gtfs2gps v1.2-0

**Major changes**
* Processing mixed detailed and frequency-based GTFS files
* Small changes in `gps_as_sflinestring()` due to new GDAL
* Function to simplify shapes

-------------------------------------------------------
# gtfs2gps v1.1-0

**Major changes**
* New function append_height(), to create height column to GPS data
* Update related to the newest versions of lwgeom and sf (#112)
* Replacing `sf_multipoint` by `sf_point` in `gps_as_sf()` to keep all the data

-------------------------------------------------------
# gtfs2gps v1.0-7

**Minor changes**
* Removing bugs found by CRAN automatic tests

-------------------------------------------------------
# gtfs2gps v1.0-5

* Launch of **gtfs2gps** v1.0-5 on [CRAN](https://CRAN.R-project.org/package=gtfs2gps) on 2020-03-16
