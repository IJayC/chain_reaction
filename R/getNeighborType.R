#' getNeighborType
#'
#' @param data_frame dataframe
#' @param row_index Integer
#' @param column_index Integer
#' @author Jayachandra N
#' @return character
#' @export
#' @description
#' 
#' 
#' @examples
#' df <- as.data.frame(matrix(NA, 5, 5))
#' getNeighborType(df, 5,5)
getNeighborType <- function(data_frame, row_index, column_index) {
  corner <- which_corner(data_frame = data_frame, 
                         row = row_index, 
                         column = column_index)
  edge <- which_edge(data_frame = data_frame, 
                     row = row_index, 
                     column = column_index)
  is_corner <- corner %in% c('top_left_corner', 
                             'top_right_corner', 
                             'bottom_left_corner', 
                             'bottom_right_corner')
  is_edge <- edge %in% c('top_most', 
                         'bottom_most', 
                         'left_most', 
                         'right_most')
  if(is_corner){
    return("corner")
  } else if(is_edge) {
    return('edge')
  } else {
    return('other')
  }
}