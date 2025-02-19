testthat::test_that("test winning moment", {
  res <- data.frame(x = "P1", y = "P1")
  testthat::expect_error(check_winning_moment(res)) 
})