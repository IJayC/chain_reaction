#' Title initialize_matrix
#'
#' @param nrow integer, number of rows to initialize dataframe
#' @param ncol  integer, number of columns to initialize dataframe
#' @param value default value to fill in each cell of initialized datataframe
#' @author Jayachandra N
#' @return dataframe, with nrow and ncol specified
#' @export
#'
#' @examples
#' initializ_matrix(5,5,NA)
initialize_matrix <- function(nrow, ncol, value="") {
  res <- as.data.frame(matrix(data = value, nrow = nrow, ncol = ncol))
  row.names(res) <- NULL
  names(res) <- NULL
  return(res)
}