input <- readLines("day_17/00_input/input.txt")
test <- readLines("day_17/00_input/test.txt")

parse_input <- function(input_text) {
  strsplit(input_text, "") |>
    unlist() |>
    as.numeric() |>
    matrix(nrow = length(input_text),
           ncol = nchar(input_text[1]),
           byrow = TRUE)
}
parsed_input <- parse_input(input)
parsed_test <- parse_input(test)

dijkstra_heavy <- function(mtx) {
  seen <- hash::hash()
  start <-  c(
    v = 0,
    x = 1,
    y = 1,
    dx = 0,
    dy = 0,
    s = 0
  )
  queue <- collections::priority_queue()
  queue$push(start)
  dirs <- list(c(0, 1), c(1, 0), c(0, -1), c(-1, 0))
  while (queue$size() > 0) {
    #Getting the values of the next node in queue
    node <- queue$pop()
    v <- node[["v"]]
    x <- node[["x"]]
    y <- node[["y"]]
    dx <- node[["dx"]]
    dy <- node[["dy"]]
    s <- node[["s"]]
    #If we have seen this node while moving in the same direction
    #and number of steps then skip, also we need to hit at least 4 steps before ending
    if (x == nrow(mtx) && y == ncol(mtx) && s >= 4) {
      return(v)

    }
    if (!is.null(seen[[paste0(node[-1], collapse = "_")]])) {
      next
    }
    seen[[paste0(node[-1], collapse = "_")]] <- TRUE
    #Check current node
    #Modify as we can take 10 consecutive steps
    if (s < 10 && (dx != 0 | dy != 0)) {
      nx <- x + dx
      ny <- y + dy
      if (nx >= 1 &&
          nx <= nrow(mtx) && ny >= 1 && ny <= ncol(mtx)) {
        new_node <-  c(
          v = v + mtx[nx, ny],
          x = nx,
          y = ny,
          dx = dx,
          dy = dy,
          s = s + 1
        )
        #Inverting the priority here so we will visit smallest node first
        queue$push(new_node, priority = -(v + mtx[nx, ny]))
      }
    }
    #Check if we've hit at least 4 steps in the same direction before turning
    if (s >= 4 || (dx == 0 & dy == 0)) {
      #Check surrounding node aka changing directions
      for (new_dir in dirs) {
        ndx <- new_dir[1]
        ndy <- new_dir[2]
        if ((ndx != dx | ndy != dy) && (ndx != -dx | ndy != -dy)) {
          nx <- x + ndx
          ny <- y + ndy
          if (nx >= 1 &&
              nx <= nrow(mtx) && ny >= 1 && ny <= ncol(mtx)) {
            new_node <-  c(
              v = v + mtx[nx, ny],
              x = nx,
              y = ny,
              dx = ndx,
              dy = ndy,
              s = 1
            )
            queue$push(new_node, priority = -(v + mtx[nx, ny]))
          }
        }
      }
    }
  }
}

dijkstra_heavy(parsed_test) == 94
#Answer Part 2: 1219
dijkstra_heavy(parsed_input)
