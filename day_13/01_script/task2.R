input <- read_file("day_13/00_input/input.txt")
test <- read_file("day_13/00_input/test.txt")
#Function to parse the input
parse_input <- function(input_text) {
  strsplit(input_text, "(\r)?\n(\r)?\n")[[1]] |>
    lapply(\(x) strsplit(x, "(\r)?\n")[[1]] |>
             strsplit("") |>
             stringi::stri_list2matrix(byrow = TRUE))
}

list_input <- parse_input(input)
list_test <- parse_input(test)
#One function to go through each row and flip the top half, trim either top
#or bottom based on whichever is shorter, and compare
search_symmetry <- function(mtx) {
  r <- NA
  for (i in 1:(nrow(mtx) - 1)) {
    #Flip top part
    top_mtx <- mtx[i:1, , drop = FALSE]
    bot_mtx <- mtx[(i + 1):nrow(mtx), , drop = FALSE]
    # Trimming off
    max_rows <- min(nrow(top_mtx), nrow(bot_mtx))
    top_mtx <- top_mtx[1:max_rows, , drop = FALSE]
    bot_mtx <- bot_mtx[1:max_rows, , drop = FALSE]
    #if the difference is off by one then we found a smudge
    if (sum(top_mtx != bot_mtx) == 1) {
      r <- i
    }
  }
  return(r)
}

find_rc <- function(mtx) {
  rows <- search_symmetry(mtx)
  #Just need to transpose mtx for column search
  cols <- search_symmetry(t(mtx))
  return(c(rows*100,cols))
}

vapply(list_test, find_rc, numeric(2)) |>
  sum(na.rm = TRUE) == 1400
#Answer Part 2: 36735
vapply(list_input, find_rc, numeric(2)) |>
  sum(na.rm = TRUE)

