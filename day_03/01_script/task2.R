library(tidyverse)
library(testthat)

input <- readLines("day_03/00_input/input.txt")
test <- readLines("day_03/00_input/test1.txt")

mat <- sapply(input, \(x) strsplit(x, ""), USE.NAMES = FALSE) |>
  stringi::stri_list2matrix(byrow = TRUE)

test_mat <- sapply(test, \(x) strsplit(x, ""), USE.NAMES = FALSE) |>
  stringi::stri_list2matrix(byrow = TRUE)

extract_gears <- function(mat) {
  all_numbers <- list()
  for (i in 1:nrow(mat)) {
    #Index of all the stars in a row
    star_indices <- grep("\\*", mat[i, ])
    #Proceed to next row if no stars found
    if (length(star_indices) > 0 ) {
      gear_number <- lapply(star_indices,
             function(star) {
               adjacent_values <-
                 mat[max(i - 1, 1):min(i + 1, nrow(mat)),
                     max(star-1, 1):min(star+1, ncol(mat))]
               #Find all the numbers around the star,
               #Return product of those numbers if there are exactly 2 numbers
              numbers <- find_numbers(mat, i, star)
              if (length(numbers) == 2) {
                prod(numbers)
              }
             })
      all_numbers <- append(all_numbers, gear_number)
    }
  }
  return(all_numbers)
}
#Given a pair of coordinates and a matrix
#find the complete number surrounding that coordinate
find_numbers <- function(mat, row, col) {
  rows <- max(row - 1, 1):min(row + 1, nrow(mat))
  cols <- max(col - 1, 1):min(col + 1, ncol(mat))
  #Find absolute position of numbers surrounding the coordinate
  adjacent_number_idx <-
    apply(mat[rows, cols],
          1,
          function(line) {
            idx = grep("[0-9]", line)
            if (length(idx) > 0) {
              idx - 2 + col
            }
          })
  numbers <- list()
  # Get the index of numbers of the 3 rows around the coordinates
  for (i in seq_along(adjacent_number_idx)) {
    has_number <- !is.null(adjacent_number_idx[[i]])
    if (has_number) {
      idx <- grep("[0-9]", mat[rows[i], ])
      grps <- cumsum(c(0, diff(idx)) != 1)
      res <- tapply(idx, grps, function(x) {
        is_valid <- any(adjacent_number_idx[[i]] %in% x)
        if (is_valid) {
          paste0(mat[rows[i], min(x):max(x)], collapse = "")
        }
      }) |> unname()
    numbers <- append(numbers, res)
    res <- NULL
    }
  }
  out <- unlist(numbers) |> as.integer()
  return(out)
}


testthat::expect_equal(
  extract_gears(test_mat) |> unlist() |> sum(),
  467835
)

#Answer Part 2: 78272573
extract_gears(mat) |> unlist() |> sum()



