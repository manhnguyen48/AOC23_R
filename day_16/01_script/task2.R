input <- readLines("./day_16/00_input/input.txt")
test <- readLines("./day_16/00_input/test1.txt")

parse_input <- function(input_text) {
  strsplit(input_text, "") |>
    stringi::stri_list2matrix(byrow = TRUE)
}
parsed_input <- parse_input(input)
parsed_test <- parse_input(test)

#Util function reusing from part 1
source("day_16/01_script/shared_function.R")
#Brute force is on the menu again :(
find_best <- function(mtx) {
  #Generate all possible starting positions
  top_down <- vapply(1:ncol(mtx), \(x) c(0,x,1,0), numeric(4))
  bottom_up <- vapply(1:ncol(mtx), \(x) c(ncol(mtx)+1,x,-1,0),numeric(4))
  left_right <- vapply(1:nrow(mtx), \(x) c(x,0,0,1), numeric(4))
  right_left <- vapply(1:nrow(mtx), \(x) c(x,nrow(mtx)+1,0,-1), numeric(4))
  possible_starts <- cbind(top_down, bottom_up, left_right, right_left)
  #Test every combinations to find the best config
  values <- apply(possible_starts, 2, \(x) count_seen(bfs_tiles(mtx, x)))
  return(values)
}

find_best(parsed_test) |> max() == 51
#Answer Part 2: 8061
find_best(parsed_input) |> max()
