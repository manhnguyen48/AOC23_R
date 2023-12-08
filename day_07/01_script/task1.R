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
      hand_name = sapply(hand, \(x) classify_hand(x)) |>
        factor(levels = c("High card", "One pair", "Two pair",
                          "Three of a kind", "Full house", "Four of a kind",
                          "Five of a kind"), ordered = TRUE)) |>
    dplyr::select(hand, bid, hand_name) |>
    tibble::rownames_to_column("id")
}
#Function to break the tie
tie_break <- function(tbl) {
  tidyr::unnest_wider(tbl, hand, names_sep = "_")|>
    #Sort ascending, tie break by next card
    dplyr::arrange(`hand_1`, `hand_2`, `hand_3`,
                   `hand_4`, `hand_5`) |>
    #Reverse the number as bigger
    dplyr::mutate(rank_within = 1:nrow(tbl)) |>
    dplyr::select(id, rank_within, bid)
}
#Rank the hand
rank_hand <- function(tbl) {
  tidyr::nest(tbl, .by = hand_name) |>
    dplyr::mutate(
      rank_tbl = lapply(data, \(x) tie_break(x)), .keep = "unused") |>
    tidyr::unnest(rank_tbl) |>
    dplyr::arrange(hand_name, as.numeric(rank_within)) |>
    tibble::rownames_to_column("rank_total") |>
    dplyr::mutate(winning = as.numeric(rank_total) * bid)
}

test_processed <- process_input(test)
input_processed <- process_input(input)

testthat::expect_equal(
  rank_hand(test_processed) |>
    pull(winning) |>
    sum(),
  6592
)
#Part 1 Answer: 251216224
rank_hand(input_processed) |>
  pull(winning) |>
