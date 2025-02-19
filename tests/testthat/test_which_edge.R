# Call
test_df <- as.data.frame(matrix(NA, 10,10))

# which_edge(test_df, 2, 2)
# which_edge(test_df, 1,3)
# which_edge(test_df, 4,3)
# which_edge(test_df, 3,4)
# which_edge(test_df, 3,1)

testthat::test_that("Test which edge..", {
  test_df <- as.data.frame(matrix(NA, 10,10))
  testthat::expect_equal(which_edge(test_df, 10, 2), "bottom_most")
  testthat::expect_equal(which_edge(test_df, 1, 2), 'top_most')
  testthat::expect_equal(which_edge(test_df, 3, 1), 'left_most')
  testthat::expect_equal(which_edge(test_df, 3, 10), 'right_most')
  testthat::expect_equal(which_edge(test_df, 2, 2), 'not_an_edge' )
})
