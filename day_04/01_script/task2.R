library(tidyverse)
library(testthat)

input <- readLines("./day_04/00_input/input.txt")
test <- readLines("./day_04/00_input/test.txt")

#Function to process data & count matches of each card
count_matches <- function(input) {
  processed_data <- tibble(
    id = stringr::str_extract(input, "(?<=Card)( )+\\d+(?=\\:)") |> trimws() |>
      as.integer(),
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
#Function to count the number of cards won
count_cards <- function(processed_data) {
  #We only need the match value of each card
  matches <- dplyr::select(processed_data, id, matches) |>
    dplyr::mutate(cards_count = 1) |>
    as.data.frame()
  for (i in 1:nrow(matches)) {
    match_count <- matches$matches[[i]]
    cards_count <- matches$cards_count[[i]]
    if (match_count != 0) {
      #Add more card values to card below the current one
      matches$cards_count[(i + 1):(i + match_count)] <-
        matches$cards_count[(i + 1):(i + match_count)] + cards_count
    }
  }
  sum(matches$cards_count)
}

testthat::expect_equal(
  count_matches(test) |> count_cards(),
  30
)
#Answer Part 2: 5329815
count_matches(input) |> count_cards()