#' @title Merge multiple GTFS feeds into a single one
#' 
#' @description Build a single GTFS by joinning together the elements of multiple GTFS feeds.
#' @param gtfs_list A list or a vector of GTFS.zip file names.
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
merge_gtfs_feeds <- function(gtfs_list){
  if(is.character(gtfs_list))
     gtfs_list <- as.list(gtfs_list)
  
  # read all fees separately
  all_feeds <- lapply(gtfs_list, function(i){
    message(paste0("GTFS '", i, "'"))
    read_gtfs(i)
  })
  
  create_new_ids <- function(i, id, files){
    values <- function(i, mfile, id)
      all_feeds[[i]][[mfile]][[id]]
    
    ids <- as.vector(unlist(lapply(files, function(mfile) values(i, mfile, id))))
    new_ids <- paste0(i, "_", seq_along(ids))
    
    for(mfile in files){  
      if(!is.null(all_feeds[[i]][[mfile]])){
        positions <- match(values(i, mfile, id), ids)
        all_feeds[[i]][[mfile]][[id]] <- new_ids[positions]
      }
    }
  }
  
  for(i in seq_along(all_feeds)){
    create_new_ids(i, "shape_id",   c("shapes", "trips"))
    create_new_ids(i, "agency_id",  c("agency", "routes"))
    create_new_ids(i, "route_id",   c("routes", "trips"))
    create_new_ids(i, "trip_id",    c("trips", "stop_times"))
    create_new_ids(i, "stop_id",    c("stop_times", "stops"))
    create_new_ids(i, "service_id", c("trips", "calendar", "calendar_dates"))
    create_new_ids(i, "trip_id",    c("trips", "frequencies", "stop_times"))
  }

  # separate 1st feed
  new_gtfs <- list()
  
  # function to extract elements in a series of lists
  extract_list_element <- function(i, element){ all_feeds[[i]][[element]] }
  
  ## piling up
  
  # 1/8 agency
  new_gtfs$agency <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'agency') %>% data.table::rbindlist(fill = TRUE)
  
  # 2/8 routes
  new_gtfs$routes <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'routes') %>% data.table::rbindlist(fill = TRUE)
  
  # 3/8 stops
  new_gtfs$stops <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'stops') %>% data.table::rbindlist(fill = TRUE)
  
  # 4/8 stop_times
  new_gtfs$stop_times <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'stop_times') %>% data.table::rbindlist(fill = TRUE)
  
  # 5/8 shapes
  new_gtfs$shapes <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'shapes') %>% data.table::rbindlist(fill = TRUE)
  
  # 6/8 trips
  new_gtfs$trips <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'trips') %>% data.table::rbindlist(fill = TRUE)
  
  # 7/8 calendar
  new_gtfs$calendar <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'calendar') %>% data.table::rbindlist(fill = TRUE)
  
  # 8/8 frequencies
  new_gtfs$frequencies <- lapply(X = seq_along(all_feeds), FUN = extract_list_element, 'frequencies') %>% data.table::rbindlist(fill =TRUE)

  if(dim(new_gtfs$frequencies)[1] == 0) new_gtfs$frequencies <- NULL

  return(new_gtfs)
}
