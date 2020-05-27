
#' @title Simplify shapes of a GTFS file
#'
#' @description Remove points from the shapes of a GTFS file in order to
#' reduce its size. It uses Douglas-Peucker algotithm internally.
#'
#' @param gtfs_data A list of data.tables read using gtfs2gps::reag_gtfs().
#' @param tol Numerical tolerance value to be used by the Douglas-Peuker algorithm.
#' The default value is 0, which means that no data will be lost.
#' @return A GTFS data whose shapes is a subset of the input data.
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#'
#' poa_simpl <- simplify_shapes(poa)
simplify_shapes <- function(gtfs_data, tol = 0){
  gtfs_sf <- gtfs_shapes_as_sf(gtfs_data)
  
  gtfs_st <- sf::as_Spatial(gtfs_sf)

  gtfs_st_simpl <- rgeos::gSimplify(gtfs_st, tol)

  # try to do something better
  IDs <- gtfs_sf$shape_id
  
  dt <- data.table::data.table()
  
  for(i in 1:length(IDs)){
    coords <- gtfs_st_simpl@lines[[i]]@Lines[[1]]@coords
    
    df <- data.table::data.table(
      shape_id = IDs[i],
      shape_pt_lat = coords[, "y"],
      shape_pt_lon = coords[, "x"],
      shape_pt_sequence = 1:(dim(coords)[1])
    )
    
    dt <- rbind(dt, df)
    
  }

  gtfs_data$shapes <- dt
  
  return(gtfs_data)
}
