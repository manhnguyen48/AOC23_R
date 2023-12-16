input <- readLines("./day_16/00_input/input.txt")
test <- readLines("./day_16/00_input/test2.txt")

parse_input <- function(input_text) {
  strsplit(input_text, "") |>
    stringi::stri_list2matrix(byrow = TRUE)
}
parsed_input <- parse_input(input)
parsed_test <- parse_input(test)
#Util function to do BFS & count tiles
source("day_16/01_script/shared_function.R")

count_seen(bfs_tiles(parsed_test, c(1, 0, 0, 1))) == 89
#Answer Part 1: 7728
count_seen(bfs_tiles(parsed_input, c(1, 0, 0, 1)))