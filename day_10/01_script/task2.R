library(igraph)
input <- readLines("day_10/00_input/input.txt")
test <- readLines("day_10/00_input/test3.txt")

parse_input <- function(input_text) {
  width <- unique(sapply(input_text, nchar))
  height <- length(input_text)
  vapply(input_text, \(x) strsplit(x, "")[[1]],
         character(width),
         USE.NAMES = FALSE) |>
    as.vector() |>
    matrix(nrow = height, ncol = width, byrow = TRUE,
           dimnames = list(c(1:height), c(1:width)))
}
processed_input <- parse_input(input)
processed_test <- parse_input(test)
#Function to convert the matrix to adjacency matrix
convert_adj <- function(mtx) {
  out <- matrix(rep(0, length(mtx) ^ 2),
                nrow = length(mtx),
                ncol = length(mtx))
  idx_mtx <- matrix(1:length(mtx), nrow = nrow(mtx), ncol = ncol(mtx))
  directions <- list(
    up = c(-1, 0),
    down = c(1, 0),
    left = c(0, -1),
    right = c(0, 1)
  )
  valid_next <- list(
    up = c("|", "F", "7"),
    down = c("|", "L", "J"),
    left = c("-", "L", "F"),
    right = c("-", "J", "7")
  )
  for (j in 1:ncol(mtx)) {
    for (i in 1:nrow(mtx)) {
      cur_val <- mtx[i, j]
      idx <- idx_mtx[i, j]
      #If dot then all the adjacency is 0
      # Otherwise we go through the neighbours
      if (cur_val != ".") {
        for (k in 1:length(directions)) {
          step <- directions[[k]]
          in_bound_row <-
            (((i + step[1]) >= 1) & (i + step[1]) <= nrow(mtx))
          in_bound_col <-
            (((j + step[2]) >= 1) & (j + step[2]) <= ncol(mtx))
          #If the step index is valid - still in bound of the matrix
          if (in_bound_row & in_bound_col) {
            name_step <- names(directions)[[k]]
            step_val <- mtx[i + step[1], j + step[2]]
            #If the step value is valid to go then update adjacency matrix
            if (step_val %in% valid_next[[name_step]]) {
              neighbour_idx <-  idx_mtx[i + step[1], j + step[2]]
              out[idx, neighbour_idx] <- 1
            }
          }
        }
      }
    }
  }
  #Convert to sparse matrix to save space
  return(as(out, "dgCMatrix"))
}
adjmtx_test <- convert_adj(processed_test)
adjmtx_input <- convert_adj(processed_input)
gc()

graph_test <-igraph::graph_from_adjacency_matrix(adjmtx_test)
graph_input <- igraph::graph_from_adjacency_matrix(adjmtx_input)

plot(graph_test, edge.arrow.size=0.5,
     vertex.size = 5,
     layout = layout.grid(graph_test,
                          nrow(processed_test),
                          ncol(processed_test)))

ray_cast <- function(graph, node, mtx) {
  print(mtx)
  # a matrix of index to keep track of
  idx_mtx <- matrix(
    1:length(mtx),
    nrow = nrow(mtx),
    ncol = ncol(mtx),
    dimnames = list(c(1:nrow(mtx)),
                    c(1:ncol(mtx)))
  )
  print(idx_mtx)
  #Get the node number (index) of all nodes within the loop
  all_paths <- igraph::all_shortest_paths(graph, node)
  max_length <- sapply(all_paths$res, \(x) length(x)) |> max()
  loop_nodes <-
    Filter(\(x) length(x) == max_length, all_paths$res) |>
    unlist() |> unique()
  enclosed <- 0
  #Ray cast to scan each line
  # If we hit the pipe even number then we're outside the loop
  # if odd then we're inside the loop
  for (i in 1:nrow(idx_mtx)) {
    hits <- 0
    #Find the last hit value in the row
    row_idx <- idx_mtx[i, , drop=TRUE]
    last_hit <- tail(row_idx[row_idx %in% loop_nodes], 1)
    for (j in 1:ncol(idx_mtx)) {
      cur_idx <- idx_mtx[i, j]
      #If we run into a loop nodes then increase hits
      if (cur_idx %in% loop_nodes) {
        hits <- hits + 1
        #Else check if the hits is even
      } else if (hits %% 2 == 1 & cur_idx < last_hit) {
        enclosed <- enclosed + 1
      }
    }
    print(glue::glue("row: {i}; hits: {hits}; enclosed: {enclosed}"))
  }
  return(enclosed)
}


ray_cast(graph_test, grep("S", processed_test), processed_test)

ray_cast(graph_input, 15337, processed_input)

