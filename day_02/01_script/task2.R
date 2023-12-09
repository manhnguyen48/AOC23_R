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
# Function to calculate the least cubes to make a game possible
# take the max number of each cubes ever seen and calculate their product
least_cube_power <- function(info_extract) {
  max_col <- apply(as.matrix(info_extract), 2, \(x) max(x, na.rm=TRUE))
  return(prod(max_col))
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
      least_power = vapply(info_extract, least_cube_power,
                      numeric(1), USE.NAMES = FALSE),
      .keep = "unused")
}

processed_test <- process_input(test)
processed_input <- process_input(input)

testthat::expect_equal(
  sum(processed_test$least_power),
  2286
)

#Answer Part 2: 56580
sum(processed_input$least_power)