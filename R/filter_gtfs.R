
#' @title Filter gtfs_data by shape if
#' @description Filter a gtfs_data data in the tidytransit format. It gets a set of
#' shape ids and returns a new gtfs_data by removing the other shape ids from the
#' shapes. It also removes the unnecessary trips, stop_times, stops, and routes
#' accordingly.
#' @param gtfs_data A tibble in the tidytransit format.
#' @param shape_ids A vector of shape_ids belonging to the shapes of the gtfs_data data.
#' @export
filter_by_shape_id <- function(gtfs_data, shape_ids){
  gtfs_data$shapes <- gtfs_data$shapes %>% dplyr::filter(shape_id %in% !!shape_ids)
  gtfs_data$trips <- gtfs_data$trips  %>% dplyr::filter(shape_id %in% !!shape_ids)
  
  trip_ids <- unique(gtfs_data$trips$trip_id)
  
  gtfs_data$stop_times <- gtfs_data$stop_times %>% dplyr::filter(trip_id %in% !!trip_ids)
  
  stop_ids <- unique(gtfs_data$stop_times$stop_id)
  
  gtfs_data$stops <- gtfs_data$stops %>% dplyr::filter(stop_id %in% !!stop_ids)
  
  route_ids <- unique(gtfs_data$trips$route_id)
  
  gtfs_data$routes <- gtfs_data$routes %>% dplyr::filter(route_id %in% !!route_ids)
  
  return(gtfs_data)
}

#shape_ids <- c("T2-1", "A141-2", "176-1")
