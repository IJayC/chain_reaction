
testthat::test_that(" test getNeighborsForThisCorner", {
  testthat::expect_equal(length(getNeighborsForThisCorner(iris, 1,1)), 2) 
  testthat::expect_equal(length(getNeighborsForThisCorner(iris, 1, 5)), 2) 
  testthat::expect_equal(length(getNeighborsForThisCorner(iris, 150, 5)), 2)
})
