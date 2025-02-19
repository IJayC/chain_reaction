testthat::test_that(" test getNeighboursForThisCell", {
  res <- getNeighboursForThisCell(iris, 1, 1)
  testthat::expect_equal(length(res), 2)
  testthat::expect_equal(res[[1]], c(1,2))
})
