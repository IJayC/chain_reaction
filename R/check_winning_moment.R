#' check_winning_moment
#'
#' @param df a data frame
#' @author Jayachandra N
#' @return text
#' @export
check_winning_moment <- function(df) {
  res <- unique(na.omit(as.vector(as.matrix(df))))
  if(length(res) == 1) {
    if(res == 'P1') {
      color <- "#CC6CE7"
      t <- "Player 1"
      m <- "Winner Winner Chicken Dinner!"
      # t <- paste0("<p style='font-size: 50px; font-family: Arial, sans-serif; color:", color, ";'>" , "Player 1", "</p>")
      # m <- paste0("<p style='font-size: 50px; font-family: Arial, sans-serif; color:", color, ";'>" , "Winner Winner Chicken Dinner!!", "</p>")
    } else {
      color <- "#7DDA58"
      t <- "Player 2"
      m <- "Winner Winner Chicken Dinner!"
      # t <- paste0("<p style='font-size: 50px; font-family: Arial, sans-serif; color:", color, ";'>" , "Player 2", "</p>")
      # m <- paste0("<p style='font-size: 50px; font-family: Arial, sans-serif; color:", color, ";'>" , "Winner Winner Chicken Dinner!!", "</p>")
    }
    
    shinyalert(title = t, 
               text = m)
  } else {
    return(res)
  }
}
