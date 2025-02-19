res <- initialize_matrix(5, 5, NA)
testthat::test_that("Test initialize_matrix!", {
  # res <- confmatrix(c(1,1,1,0), c(1,1,0,0))
  testthat::expect_equal(dim(res), c(5, 5))
})