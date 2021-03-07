library(sf)
library(data.table)
library(magrittr)
library(future.apply)
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


# calculate Distance between successive points
new_stoptimes[ , dist := geosphere::distGeo(matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2),
                                            matrix(c(data.table::shift(shape_pt_lon, type="lag"), data.table::shift(shape_pt_lat, type="lag")), ncol = 2))/1000]




#### SPEED
poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))

system.time(poa_gps <- gtfs2gps(poa))
user  system elapsed 
9.34    0.15    9.67 
7.59    0.15    7.73 


spo <- read_gtfs(system.file("extdata/saopaulo.zip", package = "gtfs2gps"))
system.time(spo_gps <- gtfs2gps(spo))
user  system elapsed 
79.97    1.71   82.67 
79.31    1.68   81.69

##### INPUT  ------------------------
  # normal
  gtfsn <- './inst/extdata/poa.zip'
  # freq based
  gtfsf <- './inst/extdata/saopaulo.zip'

emtu <- "R:/Dropbox/bases_de_dados/GTFS/SP GTFS/GTFS EMTU_20190815.zip"
  
  
##### TESTS normal fun ------------------------
  # normal data
  system.time(  normal <- gtfs2gps_dt_parallel2(emtu) ) # 61.55  secs

  # freq data
  system.time(  normfreq <- gtfs2gps_dt_parallel(gtfsf) ) # 130.50 secs
  
  
##### Coverage ------------------------

    
  
#  ERROR in shapeid 52936
  
  library(gtfs2gps)
  library(covr)
  library(testthat)
  
  function_coverage(fun=gtfs2gps::filter_day_period, test_file("tests/testthat/test_filter_day_period.R"))
  function_coverage(fun=gtfs2gps::test_gtfs_freq, test_file("./tests/testthat/test_test_gtfs_freq.R"))
  function_coverage(fun=gtfs2gps::gps_as_sflinestring, test_file("./tests/testthat/test_gps_as_sflinestring.R"))
  function_coverage(fun=gtfs2gps::gps_as_sfpoints, test_file("./tests/testthat/test_gps_as_sfpoints.R"))
  
  covr::package_coverage(path = ".", type = "tests")
  
##### Profiling function ------------------------
p <-   profvis( update_newstoptimes("T2-1@1#2146") )

p <-   profvis( b <- corefun("T2-1") )

















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



### CMD Check ----------------
# Check package errors
Sys.setenv(NOT_CRAN = "false")
devtools::check(pkg = ".",  cran = TRUE)
beepr::beep()


# build binary
system("R CMD build gtfs2gps --resave-data") # build tar.gz
# devtools::build(pkg = "gtfs2gps", path=".", binary = TRUE, manual=TRUE)

# Check package errors
# devtools::check("gtfs2gps")
system("R CMD check gtfs2gps_1.0.tar.gz")
system("R CMD check --as-cran gtfs2gps_1.0-0.tar.gz")











a <- all_feeds[[1]]$stops$stop_id

b <- all_feeds[[2]]$stops$stop_id


intersect(a,b, a)

Reduce(intersect, list(a,b,c))


all_feeds[[c(1,2)]]$stops$stop_id



spo <- system.file("extdata/saopaulo.zip", package = "gtfs2gps")
poa <- system.file("extdata/poa.zip", package = "gtfs2gps")
gtfs_list <- list(spo, poa)

# read all feeds
all_feeds <- lapply(gtfs_list, read_gtfs)


##### STOPS ----------------------

# extract stops from all GTFS feeds
stops <- sapply(all_feeds, "[", 'stops')

# extract stop ids
stop_ids <-  sapply(stops, "[[", 'stop_id')

# check inersection between ids
output_intersect <- Reduce(intersect, stop_ids)

# if there is any overlap
if( length(output_intersect) = 0){ 
  stops <- rbindlist(stops, fill = T)
}




for(i in 1:length(agency_ids)){
  stop_ids[i] <- sprintf("%s_%s", agency_ids[[i]], stop_ids[[i]])
  # route_ids[i] <- sprintf("%s_%s", agency_ids[[i]], route_ids[[i]])
  # trip_ids[i] <- sprintf("%s_%s", agency_ids[[i]], trip_ids[[i]])
  # ...
}


##### website  ----------------------
library(pkgdown)
library(usethis)

pkgdown::build_site()
