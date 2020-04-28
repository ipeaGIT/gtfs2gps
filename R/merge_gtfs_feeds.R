#' @title Merge multiple GTFS feeds into a single one
#' 
#' @description Build a single GTFS by joinning together the elements of multiple GTFS feeds.
#' @param gtfs_list A list of GTFS.zip files.
#' @return A single list of data.tables, where each index represents the respective GTFS file name.
#' @export
#' @examples
#' 
#' # get a list of GTFS feeds
#' spo <- system.file("extdata/saopaulo.zip", package = "gtfs2gps")
#' poa <- system.file("extdata/poa.zip", package = "gtfs2gps")
#' gtfs_list <- list(spo, poa)
#' 
#' new_gtfs <- merge_gtfs_feeds(gtfs_list)
#' 

merge_gtfs_feeds <- function(gtfs_list){
  
# read all fees separately
  all_feeds <- lapply(gtfs_list, read_gtfs)

  # separate 1st feed
  new_gtfs <- all_feeds[[2]]

  # function to extract elements in a series of lists
  extract_list_element <- function(i, element){ all_feeds[[i]][[element]] }


  ## piling up
  
  # 1/8 agency
  new_gtfs$agency <- lapply(X=1:length(all_feeds), FUN = extract_list_element, 'agency') %>% data.table::rbindlist(fill=T)
  
  # 2/8 routes
  new_gtfs$routes <- lapply(X=1:length(all_feeds), FUN = extract_list_element, 'routes') %>% data.table::rbindlist(fill=T)
  
  # 3/8 stops
  new_gtfs$stops <- lapply(X=1:length(all_feeds), FUN = extract_list_element, 'stops') %>% data.table::rbindlist(fill=T)
  
  # 4/8 stop_times
  new_gtfs$stop_times <- lapply(X=1:length(all_feeds), FUN = extract_list_element, 'stop_times') %>% data.table::rbindlist(fill=T)
  
  # 5/8 shapes
  new_gtfs$shapes <- lapply(X=1:length(all_feeds), FUN = extract_list_element, 'shapes') %>% data.table::rbindlist(fill=T)
  
  # 6/8 trips
  new_gtfs$trips <- lapply(X=1:length(all_feeds), FUN = extract_list_element, 'trips') %>% data.table::rbindlist(fill=T)
  
  # 7/8 calendar
  new_gtfs$calendar <- lapply(X=1:length(all_feeds), FUN = extract_list_element, 'calendar') %>% data.table::rbindlist(fill=T)
  
  # 8/8 frequencies
  new_gtfs$frequencies <- lapply(X=1:length(all_feeds), FUN = extract_list_element, 'frequencies') %>% data.table::rbindlist(fill=T)
  
  return(new_gtfs)
}
