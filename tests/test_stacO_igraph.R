library(igraph)
library(tictoc)
library(dodgr)


mydata <- structure(list(shape_id = c(52421L, 52421L, 52421L, 52421L, 52421L, 
                                      52421L, 52421L, 52421L, 52421L, 52421L, 52421L, 52421L, 52421L, 
                                      52421L, 52421L, 52421L, 52421L, 52421L, 52421L, 52421L), length = structure(c(0.191422504106197, 
                                                                                                                    0.191422504106197, 0.191422504106197, 0.191422504106197, 0.191422504106197, 
                                                                                                                    0.191422504106197, 0.191422504106197, 0.191422504106197, 0.191422504106197, 
                                                                                                                    0.191422504106197, 0.191422504106197, 0.191422504106197, 0.191422504106197, 
                                                                                                                    0.191422504106197, 0.191422504106197, 0.191422504106197, 0.191422504106197, 
                                                                                                                    0.191422504106197, 0.191422504106197, 0.191422504106197), units = structure(list(
                                                                                                                      numerator = "km", denominator = character(0)), class = "symbolic_units"), class = "units"), 
                         geometry = structure(list(structure(c(-46.5623281998182, 
                                                               -23.5213458001468), class = c("XY", "POINT", "sfg")), structure(c(-46.562221, 
                                                                                                                                 -23.52129), class = c("XY", "POINT", "sfg")), structure(c(-46.562121, 
                                                                                                                                                                                           -23.521235), class = c("XY", "POINT", "sfg")), structure(c(-46.5620233332577, 
                                                                                                                                                                                                                                                      -23.5211840000609), class = c("XY", "POINT", "sfg")), structure(c(-46.561925666591, 
                                                                                                                                                                                                                                                                                                                        -23.5211330000609), class = c("XY", "POINT", "sfg")), structure(c(-46.561828, 
                                                                                                                                                                                                                                                                                                                                                                                          -23.521082), class = c("XY", "POINT", "sfg")), structure(c(-46.5618098335317, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                     -23.5212126666783), class = c("XY", "POINT", "sfg")), structure(c(-46.5617916670273, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       -23.5213433333544), class = c("XY", "POINT", "sfg")), structure(c(-46.5617735004869, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         -23.5214740000284), class = c("XY", "POINT", "sfg")), structure(c(-46.5617553339104, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           -23.5216046667004), class = c("XY", "POINT", "sfg")), structure(c(-46.5617371672978, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             -23.5217353333702), class = c("XY", "POINT", "sfg")), structure(c(-46.5617190006492, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               -23.5218660000379), class = c("XY", "POINT", "sfg")), structure(c(-46.5617008339645, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 -23.5219966667036), class = c("XY", "POINT", "sfg")), structure(c(-46.5616826672438, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   -23.5221273333671), class = c("XY", "POINT", "sfg")), structure(c(-46.5616645004869, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     -23.5222580000284), class = c("XY", "POINT", "sfg")), structure(c(-46.5616463336941, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       -23.5223886666877), class = c("XY", "POINT", "sfg")), structure(c(-46.5616281668651, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         -23.5225193333449), class = c("XY", "POINT", "sfg")), structure(c(-46.56161, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           -23.52265), class = c("XY", "POINT", "sfg")), structure(c(-46.5617355000207, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     -23.5226427501509), class = c("XY", "POINT", "sfg")), structure(c(-46.5618610000276, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       -23.5226355002012), class = c("XY", "POINT", "sfg"))), class = c("sfc_POINT", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        "sfc"), precision = 0, bbox = structure(c(xmin = -46.5623281998182, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ymin = -23.52265, xmax = -46.56161, ymax = -23.521082), class = "bbox"), crs = structure(list(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    epsg = 4326L, proj4string = "+proj=longlat +datum=WGS84 +no_defs"), class = "crs"), n_empty = 0L), 
                         id = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", 
                                "k", "l", "m", "n", "o", "p", "q", "r", "s", "t"), speed_kmh = c(11, 
                                                                                                 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 
                                                                                                 11, 11, 11, 11)), sf_column = "geometry", agr = structure(c(shape_id = NA_integer_, 
                                                                                                                                                             length = NA_integer_, id = NA_integer_, speed_kmh = NA_integer_
                                                                                                 ), class = "factor", .Label = c("constant", "aggregate", "identity"
                                                                                                 )), row.names = c("1.13", "1.14", "1.15", "1.16", "1.17", "1.18", 
                                                                                                                   "1.19", "1.20", "1.21", "1.22", "1.23", "1.24", "1.25", "1.26", 
                                                                                                                   "1.27", "1.28", "1.29", "1.30", "1.31", "1.32"), class = c("sf", 
                                                                                                                                                                              "data.table", "data.frame"))












#### igraph -------------------------------

tic()
GraphResult <- data.frame(Source = c(NULL), 
                          Target = c(NULL), 
                          weight  = c(NULL))

for (i in 1:(dim(new_stoptimes)[1] - 1)) {
  
  TempGraphResult <- data.frame(Source = c(0), 
                                Target = c(0), 
                                weight  = c(0))
  
  TempGraphResult$Source[1] <- new_stoptimes$id[i]
  TempGraphResult$Target[1] <- new_stoptimes$id[i + 1]
  TempGraphResult$weight[1] <- new_stoptimes$dist[i]
  
  GraphResult <- rbind(GraphResult, TempGraphResult) }

MyIgraph <- graph_from_data_frame(GraphResult) 


#In this case works perfectly. But if you have more weight variables and even
#additional variables for the nodes, igraph have functions for constructing the
#igraph object

distances(MyIgraph, "c", "t") #returns 3.254183. Seems correct (0.1914225*17)
SquareMatrix <- distances(MyIgraph)


toc()

#### dodgr -------------------------------


tic()






x <- sf::st_combine (new_stoptimes) %>%
  sf::st_cast ("LINESTRING") %>%
  sf::st_sf ()

net <- weight_streetnet (x, type_col = "shape_id", id_col = "id", wt_profile = 1)
net$from_id <- mydata$id [as.integer (net$from_id)]
net$to_id <- mydata$id [as.integer (net$to_id)]

net$d_weighted <- as.numeric (mydata$length [1])
dodgr_dists (net, from = "c", to = "t") # 236.0481


net$d <- net$d_weighted
dodgr_dists (net, from = "c", to = "t") # 3.254183


toc()







