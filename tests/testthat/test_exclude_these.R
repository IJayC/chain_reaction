
testthat::test_that(" test exclude_items", {
  original_list <- list(
    a = c(1, 2, 0, 5, 10, -1, 7),
    b = c(3, 8),
    c = c(-1, 2, 4, 6)
  )
  excluded_values <- c(0, -1, 10)
  filtered_result <- exclude_items(original_list, excluded_values)
  filtered_result
  testthat::expect_equal(length(filtered_result), 1) 
  testthat::expect_equal(filtered_result, list(b=c(3, 8))) 
  testthat::expect_equal(class(filtered_result), 'list') 
})