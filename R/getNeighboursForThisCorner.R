#' getNeighborsForThisCorner
#'
#' @param data_frame data frame with n * n where n is > 0
#' @param row_index Integer 
#' @param column_index Integer
#' @author Jayachandra N
#' @return
#' @export
#'
#' @examples
#' getNeighborsForThisCorner(iris, 1, 1)
getNeighborsForThisCorner <- function(data_frame, row_index, column_index) {
  corner <- which_corner(data_frame, row_index, column_index)
  # browser()
  if(corner %in% c('top_left_corner', "top_right_corner", "bottom_left_corner", "bottom_right_corner")) {
    if(corner == "top_left_corner"){
      neightbours <- list(c(row_index + 1, column_index), c(row_index, column_index + 1))
    }
    if(corner == "top_right_corner") {
      neightbours <- list(c(row_index + 1, column_index), c(row_index, column_index - 1))
    }
    if(corner == "bottom_left_corner"){
      neightbours <- list(c(row_index -1, column_index), c(row_index, column_index + 1))
    }
    if(corner == 'bottom_right_corner') {
      neightbours <- list(c(row_index -1, column_index), c(row_index, column_index + 1))
    }
  } else {
    neightbours <- "not_a_corner, Perhaps! it's an edge of DF"
  }
  return(neightbours)
}