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

dijkstra <- function(mtx) {
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
    #Getting the values of the next node in queue by priority
    node <- queue$pop()
    v <- node[["v"]]
    x <- node[["x"]]
    y <- node[["y"]]
    dx <- node[["dx"]]
    dy <- node[["dy"]]
    s <- node[["s"]]
    #If we've hit the bottom right then exit
    if (x == nrow(mtx) && y == ncol(mtx)) {
      return(v)

    }
    #If we have seen this node while moving in the same direction
    #and number of steps then skip
    if (!is.null(seen[[paste0(node[-1], collapse = "_")]])) {
      next
    }
    seen[[paste0(node[-1], collapse = "_")]] <- TRUE
    #Check the next node in direction we're going
    #If we're moving less than 3 steps
    if (s < 3 &&
        (dx != 0 | dy != 0)) {
      nx <- x + dx
      ny <- y + dy
      #Check if our new node is still in bound
      if (nx >= 1 && nx <= nrow(mtx) && ny >= 1 && ny <= ncol(mtx)) {
        new_node <-
          c(
            #Incrementing the value by the block value
            v = v + mtx[nx, ny],
            x = nx,
            y = ny,
            dx = dx,
            dy = dy,
            #First step we take in this direction
            s = s + 1
          )
        #Inverting the priority here so we will visit smallest node first
        queue$push(new_node, priority = -(v + mtx[nx, ny]))
      }
    }
    #Check surrounding node aka changing directions
    for (new_dir in dirs) {
      ndx <- new_dir[1]
      ndy <- new_dir[2]
      #Skip if the direction is the same as current or we're stepping back where
      #we came from
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
            #This is the first step we take in this direction
            s = 1
          )
          queue$push(new_node, priority = -(v + mtx[nx, ny]))
        }
      }
    }
  }
}

dijkstra(parsed_test) == 102
#Answer Part 1: 1076
dijkstra(parsed_input)
