library(tidyverse)
library(testthat)

input <- readLines("./day_04/00_input/input.txt")
test <- readLines("./day_04/00_input/test.txt")

#Function to process data & count matches of each card
count_matches <- function(input) {
  processed_data <- tibble(
    id = stringr::str_extract(input, "(?<=Card)( )+\\d+(?=\\:)") |> trimws(),
    result = stringr::str_extract(input, "(?<=\\:).*(?=\\|)") |> trimws(),
    have = stringr::str_extract(input, "(?<=\\|).*") |> trimws()
  ) |>
    dplyr::mutate(dplyr::across(c(result, have),
                                \(x) lapply(
                                  x,
                                  \(line) strsplit(line, "( )+")[[1]] |>
                                    as.integer()
                                )),
                  matches = purrr::map2_dbl(result, have,
                                            \(x, y) sum(y %in% x)))
  return(processed_data)
}
#Function to tally up the points, first point is 1, every point after than
#doubles the point
count_points <- function(processed_data) {
  processed_data |>
    dplyr::mutate(point = purrr::map_dbl(matches,
                                         \(x) ifelse(x==0, 0, 2^(x-1)))) |>
    dplyr::pull(point) |>
    sum()
}

testthat::expect_equal(
  count_matches(test) |> count_points(),
  13
)
#Part 1 Answer: 21105
count_matches(input) |> count_points()

