#' getNeighboursForThisCell
#'
#' @param data_frame dataframe of dim n*n where n > 0
#' @param row_index Integer
#' @param column_index Integer
#' @author Jayachandra N
#' @descriptin 
#' @return
#' @export
#' 
#' @examples
#' getNeighboursForThisCell(iris, 5, 5)
getNeighboursForThisCell <- function(data_frame, row_index, column_index) {
  edge <- which_edge(data_frame, row_index, column_index)
  res <- list(
    c(row_index, column_index - 1),
    c(row_index, column_index + 1),
    c(row_index + 1, column_index),
    c(row_index - 1, column_index)
  )
  
  excluded_values <- c(0, -1, nrow(data_frame + 1), ncol(data_frame) + 1)
  valid_neighbours <- exclude_items(res, excluded_values)
  return(valid_neighbours)
}
