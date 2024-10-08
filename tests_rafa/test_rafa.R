library(sf)
library(data.table)
library(magrittr)
library(future.apply)
library(furrr)
library(roxygen2)
library(devtools)
library(usethis)
library(testthat)
library(profvis)
library(mapview)
library(Rcpp)
library(gtfs2gps)

# Update documentation
devtools::document(pkg = ".")


  
##### Coverage ------------------------

    
  
#  ERROR in shapeid 52936
  
  library(gtfs2gps)
  library(covr)
  library(testthat)
  
  function_coverage(fun=gtfs2gps::filter_day_period, test_file("tests/testthat/test_filter_day_period.R"))
  function_coverage(fun=gtfs2gps::test_gtfs_freq, test_file("./tests/testthat/test_test_gtfs_freq.R"))
  function_coverage(fun=gtfs2gps::gps_as_sflinestring, test_file("./tests/testthat/test_gps_as_sflinestring.R"))
  function_coverage(fun=gtfs2gps::gps_as_sfpoints, test_file("./tests/testthat/test_gps_as_sfpoints.R"))
  
  
a <-   function_coverage(fun=gtfs2gps::gtfs2gps, test_file("./tests/testthat/test_gtfs2gps.R"))
  
  
 a <-  covr::package_coverage(path = ".", type = "tests")
  
  
##### Profiling function ------------------------
p <-   profvis( update_newstoptimes("T2-1@1#2146") )

p <-   profvis( b <- corefun("T2-1") )












### teste de parent station --------------

library(data.table)
library(gtfs2gps)
library(gtfstools)

 system.time( gtfs <- read_gtfs(system.file("extdata/berlin.zip", package = "gtfs2gps")) )
 head(gtfs$stops)
# nrow(gtfs$trips)


  parent <- gtfs$stops$parent_station[1]
  children <- gtfs$stops$stop_id[which( gtfs$stops$parent_station == parent)]

stopsss <- gtfs$stop_times$stop_id
   
children[2] %in% stopsss
children %in% stopsss

subset(gtfs$stop_times, stop_id==children[1])
subset(gtfs$stop_times, stop_id==children[2])

stops_sf <- gtfs2gps::gtfs_stops_as_sf(gtfs)

subset(stops_sf, stop_id %in% children)

mapview(subset(stops_sf, stop_id %in% children))









# system.time( gtfs <- read_gtfs('R:/Dropbox/bases_de_dados/GTFS/Curitiba/gtfs_curitiba_muni_201609.zip'))

nrow(gtfs$trips)
nrow(gtfs$stop_times)

gtfs <- gtfs2gps::filter_single_trip(gtfs) 

nrow(gtfs$trips)
nrow(gtfs$stop_times)

n <- gtfs2gps::gtfs2gps(gtfs, method = 'nearest', parallel = T, spatial_resolution = 500,)
r <- gtfs2gps::gtfs2gps(gtfs, method = 'restrictive', parallel = T, spatial_resolution = 500)

unique(n$shape_id) %>% length() # 20
unique(r$shape_id) %>% length() # 16


gtfs2 <- use_parent_station(gtfs)

n2 <- gtfs2gps::gtfs2gps(gtfs2, method = 'nearest', parallel = T, spatial_resolution = 200)
r2 <- gtfs2gps::gtfs2gps(gtfs2, method = 'restrictive', parallel = T, spatial_resolution = 200)

unique(n2$shape_id) %>% length() # 20
unique(r2$shape_id) %>% length() # 16

###### fun
use_parent_station <- function(gtfs) {
  # check if gtfs has parent_station field
  if (!'parent_station' %in% names(gtfs$stops)) {
    return(NULL)
  }
  
  # stops which are parent_station (location_type==1) will be Parent Stations of themselves
  if ('location_type' %in% names(gtfs$stops)) {
    gtfs$stops[location_type == 1, parent_station := stop_id]
  }
  
  # stops with no parent_station will be Parent Stations of themselves
  gtfs$stops[is.na(parent_station), parent_station := stop_id]
  gtfs$stops[parent_station == "", parent_station := stop_id]
  
  # use average location of stops with the same parent_station
  gtfs$stops[, stop_lat := mean(stop_lat), by = parent_station]
  gtfs$stops[, stop_lon := mean(stop_lon), by = parent_station]
  
  # update stops replacing stop_id with parent_station
  gtfs$stops[, parent_station := as.character(parent_station)]
  gtfs$stop_times[gtfs$stops, on = 'stop_id', stop_id := i.parent_station]
  gtfs$stops[, stop_id := as.character(parent_station)]
  
  # remove duplicated stops
  gtfs$stops <- unique(gtfs$stops)
  
  return(gtfs)
}


# non parent stops
t <- subset(gtfs$stops, !is.na(gtfs$stops$parent_station))
t <- subset(t, stop_id != parent_station)

subset(gtfs$stops, parent_station=='2900' )
subset(gtfs$stops, parent_station=='2900p6' )

subset(gtfs$stop_times, stop_id =='2900' )
subset(gtfs$stop_times, stop_id =='2900p6' )

subset(gtfs$stop_times,  stop_id %in% t$parent_station )

subset(gtfs$stop_times,  stop_id %in% t$stop_id )




# How many stops have a Parent Station 
nrow(gtfs$stops[ parent_station !=""])


# How many stops without Parent Station 
nrow(gtfs$stops[ is.na(parent_station) ])

# Stops which are Parent Stations (location_type==1) will be Parent Stations of themselves
df[ location_type==1, parent_station := stop_id ]



### update package documentation ----------------
# http://r-pkgs.had.co.nz/release.html#release-check


rm(list = ls())

library(roxygen2)
library(devtools)
library(usethis)




setwd("R:/Dropbox/git/gtfs2gps")

# update `NEWS.md` file
# update `DESCRIPTION` file
# update ``cran-comments.md` file


# checks spelling
library(spelling)
devtools::spell_check(pkg = ".", vignettes = TRUE, use_wordlist = TRUE)

# Update documentation
devtools::document(pkg = ".")


# Write package manual.pdf
system("R CMD Rd2pdf --title=Package gtfs2gps --output=./gtfs2gps/manual.pdf")
# system("R CMD Rd2pdf gtfs2gps")




# Ignore these files/folders when building the package (but keep them on github)
setwd("R:/Dropbox/git_projects/gtfs2gps")


usethis::use_build_ignore("test")
usethis::use_build_ignore("prep_data")
usethis::use_build_ignore("manual.pdf")

# script da base de dados e a propria base armazenada localmente, mas que eh muito grande para o CRAN
usethis::use_build_ignore("brazil_2010.R")
usethis::use_build_ignore("brazil_2010.RData")
usethis::use_build_ignore("brazil_2010.Rd")

# Vignette que ainda nao esta pronta
usethis::use_build_ignore("  Georeferencing-gain.R")
usethis::use_build_ignore("  Georeferencing-gain.Rmd")

# temp files
usethis::use_build_ignore("crosswalk_pre.R")


# checks spelling
library(spelling)
devtools::spell_check(pkg = ".", vignettes = TRUE, use_wordlist = TRUE)



### tests ----------------

devtools::test(stop_on_failure = T)

### CMD Check ----------------
# Check package errors

# LOCAL
Sys.setenv(NOT_CRAN = "true")
devtools::check(pkg = ".",  cran = FALSE, env_vars = c(NOT_CRAN = "true"))

# CRAN
Sys.setenv(NOT_CRAN = "false")
devtools::check(pkg = ".",  cran = TRUE, env_vars = c(NOT_CRAN = "false"))












##### website  ----------------------
library(pkgdown)
library(usethis)

pkgdown::build_site()





# submit to CRAN -----------------
usethis::use_cran_comments('teste 2222, , asdadsad')

urlchecker::url_check()
devtools::check(remote = TRUE, manual = TRUE)
devtools::check_win_release()
devtools::check_mac_release()
rhub::check_for_cran(show_status = FALSE)


devtools::submit_cran()
