require(english)
require(stringi)

input <- readLines("./day_01/00_input/input.txt")

extract_number <- function(input, numbers = 1:9) {
  number_text <- english::english(numbers) |> as.character()
  pattern <- c(as.character(numbers), number_text)
  values <- c(numbers, numbers)

  # Because words might overlap, we're create a matrix containing location of first (or last)
  # match found in the string
  first_mat <- sapply(input,
                      \(x) stringi::stri_locate(x, fixed = pattern, mode = "first")[, "start"]) |>
    `rownames<-`(values) |>
    t()

  second_mat <- sapply(input,
                       \(x) stringi::stri_locate(x, fixed = pattern, mode = "last")[, "start"]) |>
    `rownames<-`(values) |>
    t()

  paste0(apply(first_mat, 1, \(x) names(which.min(x))),
         apply(second_mat, 1, \(x) names(which.max(x))))
}


test <- c(
  "two1nine" = 29,
  "eightwothree" = 83,
  "abcone2threexyz"= 13,
  "xtwone3four" = 24,
  "4nineeightseven2" = 42,
  "zoneight234" = 14 ,
  "7pqrstsixteen" = 76,
  # "eighthree" = 83,
  "sevenine" = 79
)

testthat::expect_equal(
  extract_number(names(test), 1:9),
  sum(test)
)

data.frame(
  x= input,
  y = extract_number(input, 1:9)
)

extract_number(input, 1:9) |>
  as.numeric() |>
  sum()



