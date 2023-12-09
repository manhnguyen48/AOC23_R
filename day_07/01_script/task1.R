library(tidyverse)
library(testthat)

input <- readLines("day_07/00_input/input.txt")
test <- readLines("day_07/00_input/test.txt")

#Function to classify a hand based on what's available
classify_hand <- function(hand) {
  counts <- sort(table(hand), decreasing = TRUE)
  out <- switch(paste0(as.numeric(counts[1:2]), collapse ="-"),
                "1-1" = "High card",
                "2-1" = "One pair",
                "2-2" = "Two pair",
                "3-1" = "Three of a kind",
                "3-2" = "Full house",
                "4-1" = "Four of a kind",
                "5-0" = "Five of a kind",
                "Invalid hand")
  return(out)
}
#Function to process input and classify hand type
process_input <- function(input_text) {
  tibble::tibble(
    input = input_text) |>
    tidyr::separate(input, into = c("hand", "bid"),
                    sep = " ") |>
    dplyr::mutate(
      bid = as.numeric(bid),
      hand = lapply(hand, \(x) strsplit(x, "")[[1]] |>
                      factor(levels = c(as.character(2:9),
                                        "T", "J", "Q", "K", "A"),
                             ordered = TRUE)),
      hand_number = vapply(hand, \(x) as.numeric(x) |>
                             stringr::str_pad(2,"left", "0") |>
                             paste0(collapse = ""),
                           character(1)) ,
      hand_name = sapply(hand, \(x) classify_hand(x)) |>
        factor(levels = c("High card", "One pair", "Two pair",
                          "Three of a kind", "Full house", "Four of a kind",
                          "Five of a kind"), ordered = TRUE),
      hand_rank = paste0(
        as.character(as.numeric(hand_name)),
        hand_number) |>
        as.numeric()
      ) |>
    dplyr::arrange(hand_rank) |>
    dplyr::mutate(winning = bid * row_number()) |>
    dplyr::select(hand, hand_name, hand_rank, bid, winning)
}

test_processed <- process_input(test)
input_processed <- process_input(input)

testthat::expect_equal(
  sum(test_processed$winning),
  6592
)

#Part 1 Answer: 251216224
sum(input_processed$winning)
