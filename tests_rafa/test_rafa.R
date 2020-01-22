library(sf)
library(data.table)
library(magrittr)
library(future.apply)
library(roxygen2)
library(devtools)
library(usethis)
library(profvis)
library(mapview)
library(Rcpp)
library(gtfs2gps)

# Update documentation
devtools::document(pkg = ".")


# calculate Distance between successive points
new_stoptimes[ , dist := geosphere::distGeo(matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2),
                                            matrix(c(data.table::shift(shape_pt_lon, type="lead"), data.table::shift(shape_pt_lat, type="lead")), ncol = 2))/1000]


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
  
  
##### TESTS freq fun ------------------------

    
 system.time( f <- gtfs2gps_dt_freq2(gtfsf) ) # 156.50 
  
  
  
  
#  ERROR in shapeid 52936
  

  
  
  
  
  
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




setwd("R:/Dropbox/git_projects/gtfs2gps")
setwd("..")


# Install package
# devtools::install("gtfs2gps", build_vignettes = T)
# system("R CMD INSTALL --build gtfs2gps")

# build binary
system("R CMD build gtfs2gps --resave-data") # build tar.gz
# devtools::build(pkg = "gtfs2gps", path=".", binary = T, manual=T)

# Check package errors
# devtools::check("gtfs2gps")
system("R CMD check gtfs2gps_1.0.tar.gz")
system("R CMD check --as-cran gtfs2gps_1.0.tar.gz")







# test sfheaders ---------------------------------------



library(gtfs2gps)
library(sfheaders)
library(data.table)
library(tidytransit)

# system.time( poas <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps")) )

 system.time( poa <- read_gtfs("R:/Dropbox/bases_de_dados/GTFS/Fortaleza/GTFS_fortaleza_20191002.zip"))
 poas <- poas$shapes
 poa <- poa$shapes
 

 system.time( emtu <- read_gtfs("R:/Dropbox/bases_de_dados/GTFS/SP/GTFS EMTU_20171218.zip"))
t <- emtu$shapes


data.table::fwrite(t, 'shapes_large.csv', row.names = F, col.names = T)
data.table::fwrite(poa, 'shapes_small.csv',  row.names = F, col.names = T)
data.table::fwrite(poas, 'shapes_vsmall.csv',  row.names = F, col.names = T)

# read data set
small_shape <- data.table::fread("https://raw.githubusercontent.com/rafapereirabr/data_dump/master/shapes_small.csv")



system.time( poa_sf <- gtfs_shapes_as_sf1(t) )
system.time( poa_headers <- test2(t) )# 
system.time( t <- tidytransit::shapes_as_sf(t) )


ggtfs_shapes_as_sf
gtfs_shapes_as_sf <- function(shapes){
  
  temp_shapes <- setDT(shapes)[order(shape_id, shape_pt_sequence)]
  
  temp_shapes <- setDT(temp_shapes)[,
                                    {
                                      geometry <- sf::st_linestring(x = matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2))
                                      geometry <- sf::st_sfc(geometry)
                                      geometry <- sf::st_sf(geometry = geometry)
                                    }
                                    , by = shape_id
                                    ]
    return(temp_shapes)
}





test2 <- function(shapes, crs = 4326){
   a <- setDT(shapes)[order(shape_id, shape_pt_sequence)]
  temp_shapes <- sfheaders::sf_linestring( a, linestring_id = "shape_id" )
  return(temp_shapes)
}


mbm <- microbenchmark::microbenchmark(times = 20,
                                      
                                      'dt' = { # files
                                        poa_sf <- gtfs_shapes_as_sf(poa)
                                      },
                                      ### GPKG  -------------------------------------
                                      'sfheaders' = { # files
                                        poa_headers <- test(poa)
                                        }
                                      )

ggplot2::autoplot(mbm)






system.time( s <- gtfs_stops_as_sf(poa, crs = 4326))


system.time( sh <- sf_point(poa$stops, x='stop_lon', y='stop_lat') )
head(sh)
plot(sh)
==========================================================
  
  
  
  library(sf)
library(data.table)
library(sfheaders)
library(tidytransit)


# load gtfs data
local_gtfs_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
nyc <- read_gtfs(local_gtfs_path)


system.time( t <- tidytransit::shapes_as_sf(nyc$shapes) )
system.time( g <- myf(nyc$shapes) )
system.time( h <- sfheaders::sf_linestring( nyc$shapes, linestring_id = "shape_id" ) )
system.time( h <- test2( nyc, linestring_id = "shape_id" ) )



myf <- function(shp, crs = 4326){
  temp_shapes <- setDT(shp)[,
                            {
                              geometry <- sf::st_linestring(x = matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2))
                              geometry <- sf::st_sfc(geometry)
                              geometry <- sf::st_sf(geometry = geometry)
                            }
                            , by = shape_id
                            ]
  
  temp_shapes <- sf::st_as_sf(temp_shapes, crs = crs)
  return(temp_shapes)
}




