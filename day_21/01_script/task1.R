input <- readLines("day_21/00_input/input.txt")
test <- readLines("day_21/00_input/test.txt")

parse_input <- function(input_text) {
  strsplit(input_text, "") |>
    stringi::stri_list2matrix(byrow = TRUE)
}
parsed_input <- parse_input(input)
parsed_test <- parse_input(test)

#Breadth first search to find the number of steps
bfs <- function(mtx, starting, num_steps = 64) {
  out <- hash::hash()
  queue <- collections::deque()
  queue$push(c(starting, num_steps))
  seen <- hash::hash()
  seen[[paste0(starting, collapse = "_")]] <- TRUE
  #South, North, West, East as usual
  dirs <- list(c(0,1), c(0,-1), c(-1,0), c(1, 0))
  while(queue$size() > 0) {
    next_path <- queue$popleft()
    x <- next_path[[1]]
    y <- next_path[[2]]
    s <- next_path[[3]]
    #If the steps are even, we don't need to visit this area twice
    if (s %% 2 == 0) {
      out[[paste0(x, "_", y)]] <- TRUE
    }
    #If there is no more steps we don't need to turn
    if (s == 0) {
      next
    }
    #Turning here
    for (new_dir in dirs) {
      nx <- x + new_dir[1]
      ny <- y + new_dir[2]
      #Check boundary or if we hit a boulder
      if (nx < 1 ||
          nx > nrow(mtx) ||
          ny < 1 ||
          ny > ncol(mtx) ||
          mtx[nx, ny] == "#" ||
          !is.null(seen[[paste0(nx,"_",ny)]])) {
        next
      }
      seen[[paste0(nx,"_",ny)]] <- TRUE
      queue$push(c(nx, ny, s-1))
    }
  }
  return(out)
}

bfs(parsed_test,
    which(parsed_test == "S", TRUE) |> as.numeric(),
    6) |> names() |> length() == 16

#Answer Part 1: 3858
bfs(parsed_input,
    which(parsed_input == "S", TRUE) |> as.numeric(),
    64) |> names() |> length()
