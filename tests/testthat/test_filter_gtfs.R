context("Simplify")

test_that("Arguments", {
    poa <- tidytransit::read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"),
                                  local = TRUE,
                                  geometry = TRUE,
                                  frequency = TRUE)

    expect_equal(poa$shapes$shape_id %>% unique() %>% length(), 4)

    subset <- filter_by_shape_id(poa, c("A141", "T2-1"))
    expect_equal(subset$shapes$shape_id %>% unique() %>% length(), 1)
})
