# source: 
# https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf

# x= stops_sf
# y=new_shape
  





st_snap_points = function(x, y, max_dist = 100) {
  
  # x= stops_sf
  # y= new_shape
  
  # # projection to UTM
  # st_crs(x) = 4674
  # st_crs(y) = 4674
  # x <- st_transform(x, crs=32610)
  # y <- st_transform(y, crs=32610)
  
  # get number of points
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  out = do.call(c,
                lapply(seq(n), function(i) {
                  
                # buffer 100 m ?
                  
                  nrst = sf::st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = sf::st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(sf::st_geometry(x)[i])
                  return(sf::st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  
  out <- out %>% sf::st_sf()
  out$id <- seq(n)
  return(out)
}
