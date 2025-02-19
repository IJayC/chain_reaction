testthat::test_that(" test getNeighborType", {
  df <- as.data.frame(matrix(NA, 5, 5))
  res <- getNeighborType(df, 5,5)
  testthat::expect_equal(res, 'corner')
  testthat::expect_true(is.character(res))
})
