
require(gtfs2gps)

# downloaded 25 Nov 2020
file <- "C:\\Users\\pedro\\Downloads\\BVG_VBB_bereichsscharf.zip"

sw <- read_gtfs(file)

sw2 <- gtfstools::filter_by_shape_id(sw, paste(1:20))
sw2 <- remove_invalid(sw2)

write_gtfs(sw2, "inst/extdata/berlin.zip")
