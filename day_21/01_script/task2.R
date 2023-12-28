input <- readLines("day_21/00_input/input.txt")
test <- readLines("day_21/00_input/test.txt")

parse_input <- function(input_text) {
  strsplit(input_text, "") |>
    stringi::stri_list2matrix(byrow = TRUE)
}
parsed_input <- parse_input(input)
parsed_test <- parse_input(test)

#Breadth first search to find the number of steps if grid repeats
bfs_infinite <- function(mtx, starting, num_steps = 64) {
  grid_size <- nrow(mtx)
  out <- collections::dict()
  queue <- collections::deque()
  queue$push(c(starting, 0))
  seen <- collections::dict()
  dirs <- list(c(0,1), c(0,-1), c(-1,0), c(1, 0))
  while(queue$size() > 0) {
    next_path <- queue$popleft()
    x <- next_path[1]
    y <- next_path[2]
    s <- next_path[3]
    #If the steps are even, we don't need to visit this area twice
    if ((s %% 2) == (num_steps %% 2)) {
      out$set(c(x,y), TRUE)
    }
    #If there is no more steps we don't need to turn
    if (s > num_steps) {
      next
    }
    #Turning here
    for (new_dir in dirs) {
      nx <- x + new_dir[1]
      ny <- y + new_dir[2]
      #The grid repeats so we need to adjust the new position here
      nxx <- (nx-1) %% grid_size +1
      nyy <- (ny-1) %% grid_size +1
      #check if we hit a boulder no need to worry about hitting a boundary
      if (mtx[nxx, nyy] == "#" ||
          seen$has(c(nx,ny))) {
        next
      }
      seen$set(c(nx,ny), TRUE)
      queue$push(c(nx, ny, s+1))
    }
  }
  return(out$size())
}
#Using lm to solve the quadratic formula since we can
#go over horizontal and vertical from S. Also there is a diamond shape around the
#centre. It also takes 65 steps to get to the edge of the diamond
search_grid <- function(total_steps = 26501365, mtx) {
  grid_size <- nrow(mtx)
  starting_pos <- which(mtx == "S", TRUE)[1, ]
  #First fit the first 3 values
  y1 <- bfs_infinite(mtx, starting_pos, 65)
  y2 <- bfs_infinite(mtx, starting_pos, 65 + grid_size)
  y3 <- bfs_infinite(mtx, starting_pos, 65 + grid_size * 2)
  dat <- data.frame(x = 0:2, y = c(y1, y2, y3))
  model <- lm(y ~ I(x ^ 2) + x, data = dat)
  predict(model, newdata = data.frame(x  = total_steps %/% grid_size))
}
#Answer Part 2: 636_350_496_972_143
search_grid(mtx = parsed_input)
