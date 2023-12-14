input <- readLines("day_14/00_input/input.txt")
test <- readLines("day_14/00_input/test.txt")

parse_input <- function(input_text) {
  strsplit(input_text, "") |>
    stringi::stri_list2matrix(byrow = TRUE)
}
parsed_input <- parse_input(input)
parsed_test <- parse_input(test)
#Split the array by #, then sort descending the symbols so O would come before .
#then put it back in an array
split_and_sort <- function(x) {
  out <-
    stringi::stri_split(paste0(x, collapse = ""), regex = "#")[[1]] |>
    vapply(
      \(x) {
        if (x!="") {
          strsplit(x, "")[[1]] |>
            sort(decreasing = TRUE) |>
            paste0(collapse = "")
        } else {
          return(x)
        }
      }, character(1), USE.NAMES = FALSE
    ) |>
    paste0(collapse = "#") |>
    strsplit("")
  return(out[[1]])
}
count_load <- function(mtx) {
  for (j in 1:ncol(mtx)) {
    mtx[, j] <- split_and_sort(mtx[, j])
  }
  #Get the row index of all O then subtract them from the number of row + 1
  #to get the distance to bottom
  out <- sum(nrow(mtx) + 1 - which(mtx == "O", arr.ind = TRUE)[,"row"])
  return(out)
}

count_load(parsed_test) == 136
#Answer Part 1: 112046
count_load(parsed_input)