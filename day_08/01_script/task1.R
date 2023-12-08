library(tidyverse)
library(testthat)

input <- readLines("day_08/00_input/input.txt")
test1 <- readLines("day_08/00_input/test1.txt")
test2 <- readLines("day_08/00_input/test2.txt")

#Parse network as named list
parse_network <- function(input_text) {
  tibble::tibble(input = input_text) |>
    tidyr::separate(input, into = c("from", "to"),
                    sep = " \\= ") |>
    dplyr::mutate(to = lapply(to, \(x) {
      c(stringr::str_extract(x, "(?<=\\().*(?=\\,)"),
        stringr::str_extract(x, "(?<=\\, ).*(?=\\))"))
      })) |>
    tibble::deframe()
}

input_togo <-  ifelse(strsplit(input[1], "")[[1]] == "L", 1,2)
input_network <- parse_network(input[3:length(input)])

test1_togo <- ifelse(strsplit(test1[1], "")[[1]] == "L", 1, 2)
test1_network <- parse_network(test1[3:length(test1)])

test2_togo <- ifelse(strsplit(test2[1], "")[[1]] == "L", 1, 2)
test2_network <- parse_network(test2[3:length(test2)])


traverse_nw <- function(instruction, network,
                        start_node = "AAA", end_node = "ZZZ") {
  step <- 0
  i <- 0
  current_node <- start_node
  while (current_node != end_node) {
    #Recycle instruction if we run out of steps
    if (i >= length(instruction)) {i <- 0}
    i <- i+1
    current_node <- network[[current_node]][instruction[i]]
    step <- step + 1
  }
  return(step)
}

testthat::expect_equal(
  traverse_nw(test1_togo, test1_network),
  2
)

testthat::expect_equal(
  traverse_nw(test2_togo, test2_network),
  6
)
#Part 1 Answer: 17873
traverse_nw(input_togo, input_network)