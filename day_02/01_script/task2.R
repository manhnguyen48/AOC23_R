library(tidyverse)

input <- readLines("day_02/00_input/input.txt")

colours <- c("red", "green", "blue")

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
least_cube_power <- function(info_extract) {
  power_level <- dplyr::summarise(info_extract,
                           dplyr::across(all_of(colours), \(x) max(x, na.rm=TRUE))) |>
    as.numeric() |>
    # Calculate the product of the minimum number of cubes
    prod()
  return(power_level)
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

lapply(processed_input$info_extract, \(x) least_cube_power(x)) |>
  as.numeric() |>
  sum()

