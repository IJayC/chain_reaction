#' reactoR
#'
#' @param data_frame  dataframe
#' @param cells list of vectors of row, column indices 
#' @author Jayachandra N
#' @return Nested list
#' @export
#'
#' @examples
#' cells <- list(c(1,1))
#' df <- as.data.frame(matrix(NA, 4, 4))
#' reactoR(df, cells)
reactoR <- function(data_frame, cells) {
  # get the valid neighbors
  print(length(cells))
  affected_cells <- list()
  if(length(cells) == 0) { # call reactor recursively
    print("We are in if section")
    cells <- list()
    res <- list(data_frame = data_frame, cells = list())
    return(res)
  } else {
    print("we are in else section")
    j <- lapply(cells, FUN = function(cell) {
      row_index <- cell[1]
      column_index <- cell[2]
      the_neighbours <- getNeighboursForThisCell(data_frame, row_index, column_index)
      #Share the value to neighbor
      data_frame <- distributeToNeighbours(data_frame, row_index, column_index)
      #Get neighbors_type
      neighbors_type <- lapply(the_neighbours, FUN = function(x) {
        getNeighborType(data_frame = data_frame, row_index = x[1], column_index = x[2])
      }) %>% unlist()
      
      # Get Neighbor_scores
      neighbor_scores_after_distribution <- lapply(the_neighbours, FUN = function(x) {
        return(data_frame[x[1], x[2]])
      }) %>% unlist()
      
      #Normalize values If distributed value exceeds,
      for(i in 1:length(the_neighbours)) {
        
        if(neighbors_type[i] == 'corner' && neighbor_scores_after_distribution[i] > 1){
          r <- the_neighbours[[i]][1]
          c <- the_neighbours[[i]][2]
          data_frame[r, c] <- 2
        }
        if(neighbors_type[i] == 'edge' && neighbor_scores_after_distribution[i] > 2) {
          r <- the_neighbours[[i]][1]
          c <- the_neighbours[[i]][2]
          data_frame[r, c] <- 3
        }
        if(neighbors_type[i] == 'other' && neighbor_scores_after_distribution[i] > 3){
          r <- the_neighbours[[i]][1]
          c <- the_neighbours[[i]][2]
          data_frame[r, c] <- 4
        }
      }
      # neighbors_type[x] == 'corner' && neighbor_scores_after_distribution[x] >= 2
      
      
      is_neighbor_burstable <- lapply(1:length(neighbor_scores_after_distribution),
                                      FUN = function(x) {
                                        burstable <- (neighbors_type[x] == 'corner' && neighbor_scores_after_distribution[x] >= 2) ||
                                          (neighbors_type[x] == 'edge' && neighbor_scores_after_distribution[x] >= 3) ||
                                          (neighbors_type[x] == 'other' && neighbor_scores_after_distribution[x] >= 4)
                                        if(burstable){
                                          return(list(neighbor = the_neighbours[[x]], burstable = TRUE))
                                        } else {
                                          return(list(neighbor = the_neighbours[[x]], burstable = FALSE))
                                        }
                                      })
      
      burstable_cells <- lapply(is_neighbor_burstable, function(x) {
        if(T %in% x)
          x$neighbor
        else
          NA
      })
      # browser()
      burstable_cells <- burstable_cells[!is.na(burstable_cells)]
      list(data_frame = data_frame, burstable_cells = burstable_cells, the_neighbours = the_neighbours)
    })
    temp_cells <- lapply(1:length(j), FUN = function(i) {
      item <- j[[i]]
      item$burstable_cells
    }) %>% do.call('c', .)
    the_neighbours <- lapply(1:length(j), FUN = function(i) {
      item <- j[[i]]
      item$the_neighbours
    }) %>% do.call('c', .) 
    affected_cells <- append(affected_cells, the_neighbours) 
    data_frame <- j[[length(j)]]$data_frame
    res <- list(data_frame = data_frame, cells = temp_cells, affected_cells = affected_cells)
  }
  data_frame <- res$data_frame
  cells <- unique(res$cells)
  affected_cells <- res$affected_cells
  # print(paste("affected_cells", affected_cells))
  return(list(affected_cells = affected_cells, result = reactoR(data_frame, cells)))
}