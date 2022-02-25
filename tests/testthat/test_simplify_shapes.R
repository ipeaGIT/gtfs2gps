test_that("simplify_shapes", {
    poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))

    expect_equal(poa$shapes$shape_id %>% unique() %>% length(), 4)
    expect_equal(dim(poa$shapes)[1], 1265)

    poa_simpl <- simplify_shapes(poa, 1e-5)

    expect(poa_simpl$shapes$shape_id %>% unique() %>% length(), 4)
    expect_equal(dim(poa_simpl$shapes)[1], 867)

    poa_simpl <- simplify_shapes(poa, 1e-3)

    expect(poa_simpl$shapes$shape_id %>% unique() %>% length(), 4)
    expect_equal(dim(poa_simpl$shapes)[1], 115)
})

