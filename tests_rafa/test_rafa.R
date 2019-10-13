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



# devtools::install_github("ipeaGIT/gtfs2gps")
library(gtfs2gps)
devtools::load_all('.')

# Update documentation
devtools::document(pkg = ".")


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

