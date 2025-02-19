testthat::test_that(" test distributeToNeighbours", {
  df <- as.data.frame(matrix(NA, 3, 3))
  res <- distributeToNeighbours(df, 1, 1)
  testthat::expect_equal(class(res), 'data.frame')
  testthat::expect_equal(res[1, 2], 1)
  testthat::expect_true(is.na(res[1, 1]))
  testthat::expect_equal(res[2, 1], 1)
})