# log history of gtfs2gps package development

# gtfs2gps v2.1-1

* Major changes
  * Filter functions were removed from the package because gtftools alredy implements them
  * Function `gtfs2gps` now uses `parallel = TRUE` by default.
  * Function `gtfs2gps` now uses `gtfstools::frequencies_to_stop_times()` to convert. frequency-based GTFS feeds to stop times before processing the data.

* Minor changes
  * New argument `quiet` to `gtfs2gps()`
  * Removed `readr`, `pbapply`, `lwgeom` and `magrittr` from package dependencies.


# gtfs2gps v2.0-3

* Minor changes
  * Saving units when using argument `filepath` in `gtfs2gps()`.

# gtfs2gps v2.0-1

* Major changes
  * Imports `gtfstools` package.

* Minor changes
  * Fixing CRAN error and warning related to the vignette.
  * The function `adjust_speed()` now does not change very low speed (1.000000e-12 [km/h]) because these values indicate a situation of a stopped vehicle. Closed [249](https://github.com/ipeaGIT/gtfs2gps/issues/249).


# gtfs2gps v2.0-1

* Minor changes
  * `gtfs2gps()` now prints a message alerting if there are any trips with negative speed values in the output. [Closes #172](https://github.com/ipeaGIT/gtfs2gps/issues/172).
  * Fixing small bugs in the output of gtfs2gps().


# gtfs2gps v2.0-0

* Major changes
  * `gtfs2gps()` now creates two points for a stop when arrival and departure exist. Speed and travel time are now calculated considering both departure_ and arrival_time columns.
  * The travel statistics in the output table (speed, dist, cumdist, cumtime) for a given point are now calculated in relation to the previous point. More details in the documentation of the `gtfs2gps()` function.
  * Names of the output columns of gtfs2gps() were updated
  * New function `adjust_arrival_departure()` to allow users set a minimum time for dis/embarking times at each stop.
  * New function `adjust_speed()` to fix outlier speeds and replace missing speed values with a speed set by the user or the average speed of the system. The timestamp values are updated accordingly.
  * Fixing several small errors in the output of gtfs2gps(), what breaks compatibility with previous versions of the gtfs2gps package.



# gtfs2gps v1.5-4

* Major changes
  * Fixed CRAN bugs
  * Fixed small bug that prevented creating departure times correctly


# gtfs2gps v1.5

* Major changes
  * Fixed the update of trip_number attribute. This affects the output of `gps_as_sflinestring()`. Closes #189.
  * Imports the `gtfsio` package, used in the `read_gtfs()` and `write_gtfs()` functions. Closes #191.
  * New parameter `snap_method` added to `gtfs2gps()`.

* Minor changes
  * Function `filter_single_trip()` now also filters the `stop_times` table. Closes #195.
  * Change default `spatial_resolution` of `gtfs2gps()` from 50m to 100m. Closes #202.



# gtfs2gps dev v1.4-0

* Major changes
  - Allows filtering a GTFS feed by weekday and accounts for `calendar_dates.txt` info. Closes #103, #124 and #141
  - New sample GTFS.zip data for Berlin
  - New `compress` parameter in `gtfs2gps` function so that users can save the function outputs in compressed `.RDS` format.
  - New function `remove_by_route_id()` to remove GTFS data by route ids. Closes #180
  - Stops are now snapped to shapes using a simpler and more restrictive algorithm. This help identifies when a route is has problem with `stop_sequence` in the wrong order

* Minor changes
  * Fixed a bug in filter_day_period. Closes #96
  * Fix the treatment of midnight trips. Closes #43
  * Fix linebreak gps_as_sflinestring. Closes #171
  * Warnings are now printed as messages
  * Order stop sequence before converting to GPS.
  * Additional verifications in gtfs2gps
  * Added package Hex sticker Closes #75



# gtfs2gps v1.3-2

* Major changes
  * New function to merge GTFS feeds. Closes #34
  * New pkgdown website. https://ipeagit.github.io/gtfs2gps/ . Closes #146
  * Distance of 1st point of GPS trip now start with distance zero. Closes #136

* Minor changes
  * changes parallel execution to conform new `future` standards. closes #55
  * Improve time filter in `filter_day_period`. Closes #89 and 147
  * Improved documentation of  `spatial_resolution` parameter. Closes #116


# gtfs2gps v1.3-0

* Major changes
  * Use progress bar from progressr. Closes #142
  * Handling units of measurement
  * Speeding up some algorithms


# gtfs2gps v1.2-1

* Minor changes
  * Allowing to read gtfs files without calendar/agency
  * New verifications to create a better feedback for the user


# gtfs2gps v1.2-0

* Major changes
  * Processing mixed detailed and frequency-based GTFS files
  * Small changes in `gps_as_sflinestring()` due to new GDAL
  * Function to simplify shapes


# gtfs2gps v1.1-0

* Major changes
  * New function append_height(), to create height column to GPS data
  * Update related to the newest versions of lwgeom and sf (#112)
  * Replacing `sf_multipoint` by `sf_point` in `gps_as_sf()` to keep all the data


# gtfs2gps v1.0-7

* Minor changes
  * Removing bugs found by CRAN automatic tests


# gtfs2gps v1.0-5

* Launch of **gtfs2gps** v1.0-5 on [CRAN](https://CRAN.R-project.org/package=gtfs2gps) on 2020-03-16
