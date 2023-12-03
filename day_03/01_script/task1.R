library(tidyverse)
library(testthat)

input <- readLines("day_03/00_input/input.txt")
test <- readLines("day_03/00_input/test1.txt")

mat <- sapply(input, \(x) strsplit(x, ""), USE.NAMES = FALSE) |>
  stringi::stri_list2matrix(byrow = TRUE)

test_mat <- sapply(test, \(x) strsplit(x, ""), USE.NAMES = FALSE) |>
  stringi::stri_list2matrix(byrow = TRUE)

extract_number <- function(mat) {
  valid_numbers <- list()
  #Looping through the matrix rows
  for (i in 1:nrow(mat)) {
    #Get the index of all numbers in a row
    number_indices <- grep("[0-9]", mat[i,])
    #If there's any number then we perform validation
    if (length(number_indices) > 0) {
      #Getting continuous values in the vector to form complete numbers
      groups <- cumsum(c(0, diff(number_indices)) != 1)
      # If the adjacent values satisfy condition then return number
      res <- tapply(number_indices, groups,
                    function(x) {
                      #Detect if adjacent values have anything beside number or dot
                      adjacent_values <-
                        mat[max(i - 1, 1):min(i + 1, nrow(mat)),
                            max(min(x) - 1, 1):min(max(x) + 1, ncol(mat))]
                      cond <-  any(!grepl("[0-9.]", adjacent_values))
                      if (cond) {
                        mat[i, min(x):max(x)] |>
                          paste0(collapse = "") |>
                          as.integer()
                      }
                    })
      valid_numbers <- append(valid_numbers, res)
    }
    res <- NULL
  }
  return(valid_numbers |> unlist())
}

testthat::expect_equal(extract_number(test_mat) |> sum(), 4361)

#Answer Part 1: 536202
extract_number(mat) |> sum()
