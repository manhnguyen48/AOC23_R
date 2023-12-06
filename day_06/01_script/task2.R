library(tidyverse)
library(testthat)

input <- readLines("./day_06/00_input/input.txt")
test <- readLines("day_06/00_input/test.txt")

#Process input now each line is only one big number
process_input <- function(input_text) {
  processed <- tibble::tibble(
    Time = stringr::str_extract(input_text[1], "(?<=\\:).*") |>
      stringr::str_split("( )+", simplify = TRUE) |>
      as.vector() |>
      paste0(collapse = "") |>
      as.numeric(),
    Distance = stringr::str_extract(input_text[2], "(?<=\\:).*") |>
      stringr::str_split("( )+", simplify = TRUE) |>
      as.vector() |>
      paste0(collapse = "") |>
      as.numeric()
  ) |>
    tidyr::drop_na()
  return(processed)
}
#Naive algo to count win
count_win <- function(total_time, record) {
  sapply(0:total_time,
         \(x) {(x * (total_time - x))>record}) |>
    sum()
}

test_input <- process_input(test)
main_input <- process_input(input)


testthat::expect_equal(
  count_win(test_input$Time, test_input$Distance),
  252000
)
#Part 2 Answer: 36992486 (Brute force)
count_win(main_input$Time, main_input$Distance)

