library(tidyverse)
library(testthat)

input <- readLines("day_07/00_input/input.txt")
test <- readLines("day_07/00_input/test.txt")

#Function to classify a hand
classify_hand <- function(hand) {
  counts <- sort(table(hand), decreasing = TRUE)
  num_j <- sum(counts[names(counts) == "J"])
  #If we have J but less than 4
  if (num_j <= 4 & num_j >=1) {
    new_hand <- hand
    #We just replace J with whatever the most frequent card is
    new_hand[new_hand == "J"] <- sort(table(hand, exclude = "J"),TRUE)[1] |> names()
    new_counts <- sort(table(new_hand), decreasing = TRUE)
    top_2 <- paste0(as.numeric(new_counts[1:2]), collapse = "-")
  } else {
    #Use the standard top 2
    top_2 <- paste0(as.numeric(counts[1:2]), collapse = "-")
  }
  #Proceed to classify as usual
  out <- switch(top_2,
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
                      factor(levels = c("J", as.character(2:9),
                                        "T", "Q", "K", "A"),
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
    dplyr::arrange(hand_1, hand_2, hand_3, hand_4, hand_5) |>
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

testthat::expect_equal(rank_hand(test_processed) |>
                         pull(winning) |>
                         sum(),
                       6839)
#Part 2 Answer: 250825971
rank_hand(input_processed) |>
  pull(winning) |>
  sum()