
library(sf)
library(data.table)
library(magrittr)
library(future.apply)
library(roxygen2)
library(devtools)
library(usethis)
library(profvis)

 'sistema com tela azul' %like% 'tela' 


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
  
  
  
  
  ERROR in shapeid 52936
  

  
  
  
  
  
##### Profiling function ------------------------
p <-   profvis( update_newstoptimes("T2-1@1#2146") )

p <-   profvis( b <- corefun("T2-1") )


