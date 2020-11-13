
test_that("merge_gtfs_feeds", {
  spo <- system.file("extdata/saopaulo.zip", package = "gtfs2gps")
  poa <- system.file("extdata/poa.zip", package = "gtfs2gps")
  gtfs_list <- list(spo, poa)
  
  new_gtfs <- merge_gtfs_feeds(gtfs_list)
  gtfs_vector <- c(spo, poa)
  
  spo <- read_gtfs(spo)
  poa <- read_gtfs(poa)
  
  for(i in names(new_gtfs)[-8]){ # all except frequencies
    expect_equal(dim(new_gtfs[[i]])[1], dim(spo[[i]])[1] + dim(poa[[i]])[1])
  }

  expect_equal(dim(new_gtfs$frequencies)[1], dim(spo$frequencies)[1])


  new_gtfs_v <- merge_gtfs_feeds(gtfs_vector)

  for(i in names(new_gtfs_v)){
    expect_equal(dim(new_gtfs[[i]])[1], dim(new_gtfs_v[[i]])[1])
  }
  
  expect_equal(dim(new_gtfs$frequencies)[1], dim(spo$frequencies)[1])
  
  
  poa <- system.file("extdata/poa.zip", package = "gtfs2gps")
  gtfs_list <- list(poa, poa)
  
  result <- merge_gtfs_feeds(gtfs_list)
  
  poa2 <- read_gtfs(poa)
  
  for(i in names(result)){
      expect(dim(poa2[[i]])[1] * 2 == dim(result[[i]])[1], paste("Problem in ", i))
  }
})
