library(tidyverse)
library(testthat)

input <- readLines("day_08/00_input/input.txt")
test3 <- readLines("day_08/00_input/test3.txt")

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
#Start from a node and stop when we found a node ending with Z
traverse_nw_pattern <- function(start_node, instruction, network) {
  step <- 0
  i <- 0
  current_node <- start_node
  #Stop if we've landed on a node ending with Z
  while (!grepl(".*Z$", current_node)) {
    #Recycle instruction if we run out of steps
    if (i >= length(instruction)) {i <- 0}
    i <- i+1
    current_node <- network[[current_node]][instruction[i]]
    step <- step + 1
  }
  return(step)
}
#The minimum step we need to take is the least common multiple of the steps
#for each starting node to end on a node ending in Z
shortest_seq <- function(starting_nodes, ...) {
  values <- vapply(starting_nodes,
         \(x) traverse_nw_pattern(start_node = x, ...),
         FUN.VALUE = numeric(1),
         USE.NAMES = TRUE)
  Reduce(pracma::Lcm, values)
}

input_togo <-  ifelse(strsplit(input[1], "")[[1]] == "L", 1,2)
input_network <- parse_network(input[3:length(input)])

test3_togo <- ifelse(strsplit(test3[1], "")[[1]] == "L", 1,2)
test3_network <- parse_network(test3[3:length(test3)])

testthat::expect_equal(
  shortest_seq(grep(".*A$", names(test3_network), value = TRUE),
               test3_togo,
               test3_network),
  6
)

options(scipen = 999)
#Part 2 Answer: 15_746_133_679_061
shortest_seq(grep(".*A$", names(input_network), value = TRUE),
             input_togo,
             input_network)