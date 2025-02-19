#' Title which_edge
#'
#' @param data_frame dataframe
#' @param row numeric, value <= nrow > 0
#' @param column numeric, value <= ncol > 0
#' @author Jayachandra N
#' @return charater, type of the edge
#' @export
#'
#' @examples
#' which_edge(iris, 1, 2)
which_edge <- function(data_frame, row, column) {
  corner <- which_corner(data_frame, row, column)
  if(corner == 'not_a_corner' && sum(c(column,row) %in% c(dim(data_frame), 1)) >= 1) {
    if(column %in% dim(data_frame)){
      return("right_most")
    } else if(column == 1) {
      return("left_most")
    } else if (row %in% dim(data_frame)) {
      return("bottom_most")
    } else if(row == 1) {
      return("top_most")
    }
  } else {
    return("not_an_edge")
  }
}