#' Title which_corner
#'
#' @param data_frame dataframe
#' @param row numeric, value <= nrow > 0
#' @param column numeric, value <= ncol > 0
#' @author Jayachandra N
#' @return character, corner of dataframe
#' @export
#'
#' @examples
#' which_corner(iris, 150, 5)
which_corner <- function(data_frame, row, column) {
  if(row == 1 && column == 1) {
    return('top_left_corner')
  } else if(row == 1 && column == ncol(data_frame)) {
    return('top_right_corner')
  } else if(row == nrow(data_frame) && column == 1) {
    return('bottom_left_corner')
  } else if(row == nrow(data_frame) && column == ncol(data_frame)) {
    return('bottom_right_corner')
  } else {
    return('not_a_corner')
  }
}