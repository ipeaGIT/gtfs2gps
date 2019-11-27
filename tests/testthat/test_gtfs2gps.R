context("gtfs2gps")

test_that("gtfs2gps", {
    poa <- system.file("extdata/poa.zip", package="gtfs2gps")

    poa_gps <- read_gtfs(poa) %>%
      filter_week_days() %>%
      gtfs2gps(progress = FALSE)

    #poa_shape <- read_gtfs(poa) %>% gtfs_shapes_as_sf()
    #plot(poa_shape)
    #poa_gps_shape <- gps_as_sf(poa_gps)
    #plot(poa_gps_shape)
    #write_sf(poa_shape, "poa_shape.shp")
    #write_sf(poa_gps_shape, "poa_gps_shape.shp")
    
    my_dim <- dim(poa_gps)[1]
    expect(my_dim %in% c(303851, 303697), paste("length of gtfs incorrect:", my_dim))

    my_length <- length(poa_gps$dist[which(!poa_gps$dist < 15)])
    expect(my_length %in% c(21, 77), paste("incorrect number of distances greater than 15m:", my_length))
    
    expect_equal(sum(poa_gps$dist), 4065814, 0.001)
    
    expect_true(all(names(poa_gps) %in% 
      c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat",
        "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))
    
    expect_true(all(!is.na(poa_gps$dist)))

    expect_true(all(poa_gps$dist > 0))
    expect_true(all(poa_gps$speed > 0))
    expect_true(all(poa_gps$cumtime > 0))
    # save into file
    poa_gps <- gtfs2gps(poa, progress = FALSE, filepath = ".")
    expect_null(poa_gps)
    
    files <- list.files(".", pattern = "\\.txt$")
    names <- gsub('.{4}$', '', files)
    
    poa_shape <- gtfs_shapes_as_sf(read_gtfs(poa))
    expect_setequal(poa_shape$shape_id, names)
    
    file.remove(files)
    
    # run with a larger dataset
    test_sp <- function(){
      sp <- system.file("extdata/saopaulo.zip", package="gtfs2gps")
  
      expect_error(gtfs2gps(sp, continue = TRUE), "Cannot use argument 'continue' without passing a 'filepath'.", fixed = TRUE)
  
      sp_gps <- read_gtfs(sp) %>%
        filter_week_days() %>%
        filter_single_trip() %>%
        gtfs2gps(cores = 2, progress = FALSE)
  
      expect_true(all(names(sp_gps) %in% 
        c("trip_id", "route_type", "id", "shape_pt_lon", "shape_pt_lat",
          "departure_time", "stop_id", "stop_sequence", "dist", "shape_id", "cumdist", "speed", "cumtime")))
  
      my_dim <- dim(sp_gps)[1]
      expect(my_dim %in% c(20418074, 20418493, 19997007), paste("Wrong dim:", my_dim))
      
      expect_true(all(sp_gps$dist > 0))
      expect_true(all(sp_gps$speed > 0))
      expect_true(all(sp_gps$cumtime > 0))
    }
})
