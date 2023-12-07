library(tidyverse)
library(testthat)

input <- readLines("./day_06/00_input/input.txt")
test <- readLines("day_06/00_input/test.txt")

#Process input
process_input <- function(input_text) {
  processed <- tibble::tibble(
    Time = stringr::str_extract(input_text[1], "(?<=\\:).*") |>
      stringr::str_split("( )+", simplify = TRUE) |>
      as.numeric(),
    Distance = stringr::str_extract(input_text[2], "(?<=\\:).*") |>
      stringr::str_split("( )+", simplify = TRUE) |>
      as.numeric()
  ) |>
    tidyr::drop_na()
  return(processed)
}
#Naive algo to count win
count_win <- function(total_time, record) {
  sum((c(1:(total_time-1)) * (total_time-c(1:(total_time-1)))) > record)
}
test_input <- process_input(test)
main_input <- process_input(input)

testthat::expect_equal(
  purrr::map2_dbl(
    test_input$Time,
    test_input$Distance,
    \(x,y) count_win(x,y)
  ) |> prod(),
  288
)
#Part 1 Answer: 252000
purrr::map2_dbl(
  main_input$Time,
  main_input$Distance,
  \(x,y) count_win(x,y)
) |> prod()