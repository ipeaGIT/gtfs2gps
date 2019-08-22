library(tidytransit)
library(magrittr)
library(dplyr)
library(purrr)
library(sf)
library(units)
library(ggplot2)
library(vein)
library(tmap)

# 1 - LOADING DATA

local_gtfs_path <- system.file("extdata", 
                               "google_transit_nyc_subway.zip", 
                               package = "tidytransit")

nyc <- tidytransit::read_gtfs(local_gtfs_path, 
                 local=TRUE,
                 geometry=TRUE,
                 frequency=TRUE)

plot(nyc)

# 2 - CONVERTING TO VEIN FORMAT

splitted <- nyc$shapes %>%
    split(.$shape_id)

data <- splitted %>%
    map(~ select(., shape_pt_lon, shape_pt_lat) %>%
            as.matrix %>%
            st_linestring) %>%
    st_sfc(crs = 4326) %>%
    st_transform(crs = 2263)

plot(data)

mdata <- data.frame(
        geom = data,
        id = names(splitted),
        length = data %>% st_length() %>% set_units(km),
        stringsAsFactors = FALSE) %>%
    st_sf

# compute how many vehicles travel each line per day
mytable <- nyc$trips$shape_id %>% table %>% as.data.frame
rownames(mytable) <- mytable[,1]
mdata$freq <- mytable[mdata$id, "Freq"] / 24 %>% set_units("1/h")
mdata$freq[is.na(mdata$freq)] <- 0

# 3 - COMPUTE EMISSIONS USING VEIN

bus1 <- vein::age_hdv(x = mdata$freq)
bus1

# TODO: use stop times to compute speed between each pair

lef <- vein::ef_cetesb("CO", "UB")
E_CO <- vein::emis(veh = bus1, lkm = mdata$length, ef = lef) # speed? units of measurement?
E_CO

E_CO[,"V50"] <- 0 # TODO: I had to manually remove some NA values

sum(E_CO) # unit of measurement?

e_CO <- st_sf(E_CO, geometry = mdata$geometry)

g <- vein::make_grid(mdata, 5000)

gE_CO <- vein::emis_grid(e_CO, g) # V1...V50 -> age_1...age_50
plot(gE_CO[,"V1"])

gE_CO <- gE_CO %>% mutate(
    Total = reduce(select(as.data.frame(.), starts_with("V")), `+`)
)

sum(gE_CO$Total) # Unit of measurement is missing

# 4 - PLOTTING OUTPUT

rdPu <- RColorBrewer::brewer.pal(8, "RdPu")
cuts <- c(0, 0.1, 1:8*100)
tm_shape(gE_CO) +
    tm_fill(col = "Total", palette = rdPu, breaks = cuts) + 
    tm_layout(legend.position = c("left", "top")) +
    tm_shape(data) +
    tm_lines(lwd = 1, col = "gray")
