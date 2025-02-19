cells <- list(c(1,1))
df <- as.data.frame(matrix(NA, 4, 4))
reactoR(df, cells)

testthat::test_that(" test reactoR", {
  cells <- list(c(1,1))
  df <- as.data.frame(matrix(NA, 4, 4))
  res <- reactoR(df, cells)
  testthat::expect_true(is.list(res))
  testthat::expect_equal(length(res), 2)
  names(res)
  testthat::expect_identical(names(res), c('affected_cells', 'result'))
})