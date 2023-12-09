input <- readLines("day_02/00_input/input.txt")
test <- readLines("day_02/00_input/test.txt")

colours <- c("red", "green", "blue")

# Function to extract number of cube number from each set in a data frame
tbl_cube <- function(game) {
  lapply(game,
         function(set){
           tibble(
             red = stringr::str_extract(set, "\\d+(?= red)"),
             green = stringr::str_extract(set, "\\d+(?= green)"),
             blue = stringr::str_extract(set, "\\d+(?= blue)")
           )}) |>
    dplyr::bind_rows() |>
    dplyr::mutate(dplyr::across(all_of(colours), as.integer))
}
# Function to check if the info of a game match a specified condition
# Calculate the max number of cube seen and compare that to a specified set
filter_condition <- function(info_extract) {
  max_col <- apply(as.matrix(info_extract), 2, \(x) max(x, na.rm=TRUE))
  cond <- all(max_col <= c(red = 12, green = 13, blue = 14))
  return(cond)
}
#Function to process input
process_input <- function(input_txt) {
  tibble::tibble(raw_input = input_txt) |>
    dplyr::mutate(
      id = stringr::str_extract(raw_input, "(?<=Game ).*(?=\\:)") |>
        as.integer() ,
      info = stringr::str_extract(raw_input, "(?<=\\: ).*") |>
        stringr::str_split("\\;"),
      info_extract = lapply(info, tbl_cube),
      passed = vapply(info_extract, filter_condition,
                      logical(1), USE.NAMES = FALSE),
      .keep = "unused")
}

processed_test <- process_input(test)
processed_input <- process_input(input)

testthat::expect_equal(
  subset(processed_test, passed, select = id) |> sum(),
  8
)

#Answer Part 1: 2727
subset(processed_input, passed, select = id) |> sum()