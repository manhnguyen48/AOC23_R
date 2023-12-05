library(tidyverse)
library(testthat)

input <- readLines("day_05/00_input/input.txt")
test <- readLines("day_05/00_input/test.txt")

seeds <- strsplit(stringr::str_extract(input[[1]],
                                       pattern = "(?<=\\: ).*"), " ")[[1]] |>
  as.numeric()

test_seeds <- strsplit(stringr::str_extract(test[[1]],
                                            pattern = "(?<=\\: ).*"), " ")[[1]] |>
  as.numeric()

#Function to extract the numbers into tibble
extract_numbers <- function(number_set) {
  strsplit(number_set, " ") |>
    lapply(\(x) {
      numbers <- as.numeric(x)
      tibble::tibble(to = numbers[[1]],
                     from = numbers[[2]],
                     range = numbers[[3]])
    }) |>
    dplyr::bind_rows()
}
#Function to extract the map data
parse_maps <- function(input_map) {
  #Get all map names and their location
  map_names <- grep("[a-z]", input_map, value = TRUE)
  map_names_idx <- c(grep("[a-z]",input_map), length(input_map))
  number_set_idx <- lapply(1:(length(map_names_idx)-1),
                           function(i) {
                             if (i+1 != length(map_names_idx)) {
                              seq(map_names_idx[i]+1, map_names_idx[i+1]-2)
                             } else {
                               seq(map_names_idx[i]+1, length(input_map))
                             }
                           }) |>
    setNames(map_names)
  out <- lapply(number_set_idx,
                \(x) extract_numbers(input_map[x]))
  return(out)
}
#Function to convert input given a dictionary
convert_input <- function(input_number, dictionary) {
  conversion <- dplyr::filter(dictionary,
                              from <= input_number,
                              (from + range - 1) >= input_number)
  if (nrow(conversion) == 0) {
    output_number <- input_number
  } else if (nrow(conversion) == 1) {
    output_number <- conversion[["to"]] + input_number - conversion[["from"]]
  } else {
    stop("We have more than 1 row of conversion")
  }
  return(output_number)
}
#Chaining the conversion across all the maps
#They're sequential in map info
chain_convert <- function(initial_seed, map_info) {
  current_number <- initial_seed
  for (i in seq_along(map_info)) {
    current_number <- convert_input(current_number, map_info[[i]])
  }
  return(current_number)
}

test_map_info <- parse_maps(test[2:length(test)])

testthat::expect_equal(sapply(test_seeds,
                              \(x) chain_convert(x, test_map_info)) |>
                         min(), 35)

map_info <- parse_maps(input[2:length(input)])

# Answer Part 1: 226172555
converted_seeds <- sapply(seeds, \(x) chain_convert(x, map_info))
min(converted_seeds)