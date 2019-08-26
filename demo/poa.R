
poa <- tidytransit::read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"),
                       local = TRUE,
                       geometry = TRUE,
                       frequency = TRUE)

plot(poa)
