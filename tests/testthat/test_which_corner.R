# Call
test_df <- as.data.frame(matrix(NA, 10,10))
# which_corner(test_df, 10, 10)
# which_corner(test_df, 4, 1)
# which_corner(test_df, 1, 4)
# which_corner(test_df, 1, 1)


testthat::test_that("Test which corner..!", {
  test_df <- as.data.frame(matrix(NA, 10,10))
  testthat::expect_equal(which_corner(test_df, 10, 10), "bottom_right_corner")
  testthat::expect_equal(which_corner(test_df, 10, 1), 'bottom_left_corner')
  testthat::expect_equal(which_corner(test_df, 1, 10), 'top_right_corner')
  testthat::expect_equal(which_corner(test_df, 1, 1), 'top_left_corner')
  testthat::expect_equal(which_corner(test_df, 2, 2), 'not_a_corner' )
})