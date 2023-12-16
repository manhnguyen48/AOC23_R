library(collections)
library(hash)
bfs_tiles <- function(mtx, start) {
  queue <- collections::deque()
  queue$push(start)
  #Hashing entire vector as we could hit the same node in different direction
  seen <- hash::hash()
  while (queue$size() > 0) {
    coords_list <- queue$popleft()
    i <- coords_list[1]
    j <- coords_list[2]
    x <- coords_list[3]
    y <- coords_list[4]
    i <- i + x
    j <- j + y
    if (i < 1 || i > nrow(mtx) || j < 1 || j > ncol(mtx)) {
      next #If we hit the edge of matrix then skip
    }
    symbol <- mtx[i, j]
    #First case is where we keep the same direction of travel
    if (symbol == "." ||
        (y != 0 && symbol == "-") ||
        (x != 0 && symbol == "|"))
    {
      new_coords <- c(i, j, x, y)
      if (is.null(seen[[paste0(new_coords, collapse = "_")]])) {
        seen[[paste0(new_coords, collapse = "_")]] <- TRUE
        queue$push(new_coords)
      }
      #Second if we hit 90 degree mirrors then adjust directions
    } else if (symbol == "\\" || symbol == "/") {
      new_dir <- if (symbol == "\\") rev(c(x, y)) else rev(-c(x, y))
      x <- new_dir[1]; y <- new_dir[2]
      new_coords <- c(i, j, x, y)
      if (is.null(seen[[paste0(new_coords, collapse = "_")]])) {
        seen[[paste0(new_coords, collapse = "_")]] <- TRUE
        queue$push(new_coords)
      }
      #If we hit "|" or "-" then branch out 2 directions
    } else {
      potential_reflect <- switch(symbol,
                                  "|" = list(c(1, 0), c(-1, 0)),
                                  "-" = list(c(0, 1), c(0, -1)))
      for (new_dir in potential_reflect) {
        x <- new_dir[1]; y <- new_dir[2]
        new_coords <- c(i, j, x, y)
        if (is.null(seen[[paste0(new_coords, collapse = "_")]])) {
          seen[[paste0(new_coords, collapse = "_")]] <- TRUE
          queue$push(new_coords)
        }
      }
    }
  }
  return(seen)
}
#convert to matrix for easier processing &
#Only getting the unique i and j (tile coordinates)
count_seen <- function(seen_hash) {
  if (length(names(seen_hash))>0) {
    out <- strsplit(names(seen_hash), "_") |>
      lapply(as.numeric) |> unlist() |>
      matrix(ncol = 4, byrow= TRUE)
    out <- unique(out[,1:2]) |> nrow()
  } else {
    out <- 0
  }
  return(out)
}
