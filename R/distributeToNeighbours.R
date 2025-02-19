#' distributeToNeighbours
#'
#' @param data_frame dataframe
#' @param row_index Integer
#' @param column_index  Integer
#' @author Jayachandra N
#'
#' @return dataframe
#' @export
#'
#' @examples
#' df <- as.data.frame(matrix(NA, 3, 3))
#' distributeToNeighbours(df, 1, 1)
distributeToNeighbours <- function(data_frame, row_index, column_index) {
  neighbours <- getNeighboursForThisCell(data_frame, row_index, column_index)
  for( j in neighbours) {
    current_value <- data_frame[j[1], j[2]]
    
    if(is.data.frame(current_value))
      current_value <- unlist(current_value)
    
    if(is.na(current_value)) {
      data_frame[j[1], j[2]] <- 1
    } else {
      data_frame[j[1], j[2]] <- current_value + 1
    }
  }
  data_frame[row_index, column_index] <- NA
  return(data_frame)
}
