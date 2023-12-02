library(tidyverse)

input <- readLines("day_02/00_input/input.txt")

colours <- c("red", "green", "blue")

# Function to extract number of cube number from each set
tbl_cube <- function(game) {
  lapply(game,
         function(set){
           tibble(
             red = stringr::str_extract(set, "\\d+(?= red)"),
             green = stringr::str_extract(set, "\\d+(?= green)"),
             blue = stringr::str_extract(set, "\\d+(?= blue)")
           )
         }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(dplyr::across(all_of(colours), as.integer))
}

# Function to check if the info of a game match a specified condition
# Calculate the max number of cube seen and compare that to a specified set
filter_condition <- function(info_extract) {
  cond <- dplyr::summarise(info_extract,
                   dplyr::across(all_of(colours), \(x) max(x, na.rm=TRUE))) |>
    dplyr::mutate(
      red  = red <= 12,
      green = green  <= 13,
      blue = blue <= 14
    ) |>
    all()
  return(cond)
}


processed_input <- tibble(
  raw_input = input,
) |>
  mutate(
    id = str_extract(raw_input, "(?<=Game ).*(?=\\:)") |> as.integer() ,
    info = str_extract(raw_input, "(?<=\\: ).*") |>
      str_split("\\;"),
    num_sets = str_count(raw_input, ";"),
    info_extract = lapply(info, tbl_cube),
    .keep = "unused")



processed_input |>
  mutate(passed = lapply(info_extract, filter_condition) |> as.logical()) |>
  filter(passed) |>
  pull(id) |>
  sum()


