library(tidytransit)
library(magrittr)
library(dplyr)
library(purrr)
library(sf)
library(units)
library(ggplot2)
library(vein)
library(tmap)

setwd("C:/Users/pedro/github/gtfs2vein")

# 1 - LOADING DATA

local_gtfs_path <- "GTFS_POA_20190415.zip"

gtfs <- tidytransit::read_gtfs(local_gtfs_path, 
                 local=TRUE,
                 geometry=TRUE,
                 frequency=TRUE)

plot(gtfs)

# 2 - CONVERTING TO VEIN FORMAT

splitted <- gtfs$shapes %>%
    split(.$shape_id)

data <- splitted %>%
    map(~ select(., shape_pt_lon, shape_pt_lat) %>%
            as.matrix %>%
            st_linestring) %>%
    st_sfc(crs = 4326) %>% # latlong WGS84
    st_transform(crs = 2263) # TODO: find better projection for POA

plot(data)

mdata <- data.frame(
        geom = data,
        id = names(splitted),
        length = data %>% st_length() %>% set_units(km),
        stringsAsFactors = FALSE) %>%
    st_sf

# compute how many vehicles travel each line per day
mytable <- gtfs$trips$shape_id %>% table %>% as.data.frame
rownames(mytable) <- mytable[,1]
mdata$freq <- mytable[mdata$id, "Freq"] / 24 %>% set_units("1/h")
mdata$freq[is.na(mdata$freq)] <- 0 # TODO: remove these lines from the final data?

# 3 - COMPUTE EMISSIONS USING VEIN

bus1 <- vein::age_hdv(x = mdata$freq)
bus1
# What could I use to say that all vehicles are new?

# TODO: use GTFS stop times to compute speed between each pair

lef <- vein::ef_cetesb("CO", "UB")
E_CO <- vein::emis(veh = bus1, lkm = mdata$length, ef = lef)
# speed: in km/h? All vehicles travel at the same speed?
E_CO

# emissions in g of CO per day?
# Documentation says it is g/h and then g/day
# Argument profile?

# V1...V50 means age_1...age_50?
# row 10 -> only zeros?
# column V50 -> only NA?

E_CO[,"V50"] <- 0 # TODO: remove this line to manually remove NA values

sum(E_CO) # is it possible to allow sum to return units of measurement?

e_CO <- st_sf(E_CO, geometry = mdata$geometry)

g <- vein::make_grid(mdata, 5000)

gE_CO <- vein::emis_grid(e_CO, g)
# gE_CO does not have unit of measurement
plot(gE_CO[,"V1"])

gE_CO <- gE_CO %>% mutate(
    Total = reduce(select(as.data.frame(.), starts_with("V")), `+`)
)

sum(gE_CO$Total)

# 4 - PLOTTING OUTPUT

rdPu <- RColorBrewer::brewer.pal(8, "RdPu")
cuts <- c(0, 0.1, 1:7*200)
tm_shape(gE_CO) +
    tm_fill(col = "Total", palette = rdPu, breaks = cuts) + 
    tm_layout(legend.position = c("right", "top")) +
    tm_shape(data) +
    tm_lines(lwd = 1, col = "gray")
