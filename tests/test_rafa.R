
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

  
  
  
##### TESTS normal fun ------------------------
  # normal data
  normal <- gtfs2gps_dt_parallel(gtfsn)

  # freq data
  normfreq <- gtfs2gps_dt_parallel(gtfsf)
  
  
##### TESTS freq fun ------------------------

    
  f <- gtfs2gps_dt_freq2(gtfsf)
  
  
  
  
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
setwd("..")

# update `NEWS.md` file
# update `DESCRIPTION` file
# update ``cran-comments.md` file


# checks spelling
library(spelling)
devtools::spell_check(pkg = "gtfs2gps", vignettes = TRUE, use_wordlist = TRUE)

# Update documentation
devtools::document(pkg = "gtfs2gps")


# Write package manual.pdf
system("R CMD Rd2pdf --title=Package geobr --output=./geobr/manual.pdf")
# system("R CMD Rd2pdf geobr")




# Ignore these files/folders when building the package (but keep them on github)
setwd("R:/Dropbox/git_projects/geobr")


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




setwd("R:/Dropbox/git_projects/geobr")
setwd("..")


# Install package
# devtools::install("geobr", build_vignettes = T)
# system("R CMD INSTALL --build geobr")

# build binary
system("R CMD build geobr --resave-data") # build tar.gz
# devtools::build(pkg = "geobr", path=".", binary = T, manual=T)

# Check package errors
# devtools::check("geobr")
system("R CMD check geobr_1.0.tar.gz")
system("R CMD check --as-cran geobr_1.0.tar.gz")

