input <- read_file("day_13/00_input/input.txt")

test <- read_file("day_13/00_input/test.txt")

parse_input <- function(input_text) {
  strsplit(input_text, "(\r)?\n(\r)?\n")[[1]] |>
    lapply(\(x) strsplit(x, "(\r)?\n")[[1]] |>
             strsplit("") |>
             stringi::stri_list2matrix(byrow = TRUE))
}

list_input <- parse_input(input)
list_test <- parse_input(test)

search_symmetry <- function(mtx) {
  for (i in 2:(nrow(mtx)-1)) {

    height <- min(nrow(mtx)-i, i)

    top_mtx <- mtx[i:(height), ]
    bot_mtx <- mtx[(i+1):nrow(mtx), ]


  }
}
