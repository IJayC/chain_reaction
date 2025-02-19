#' exclude_items
#'
#' @param my_list List
#' @param excluded_values vector
#' @author Jayachandra N
#' @return
#' @export
#'
#' @examples
#' original_list <- list(a = c(1, 2, 0, 5, 10, -1, 7), b = c(3, 8),c = c(-1, 2, 4, 6))
#' excluded_values <- c(0, -1, 10)
#' filtered_result <- exclude_items(original_list, excluded_values)
exclude_items <- function(my_list, excluded_values) {
  filtered_list <- lapply(my_list, function(sub_list) {
    if (any(sub_list %in% excluded_values)) {
      NULL  # Exclude the entire sub-list
    } else {
      sub_list
    }
  })
  filtered_list <- filtered_list[!sapply(filtered_list, is.null)]  # Remove NULL sub-lists
  return(filtered_list)
}