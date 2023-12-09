input <- readLines("./day_01/00_input/input.txt")
test <- readLines("day_01/00_input/test.txt")

extract_number <- function(input) {
  paste0(regmatches(input, regexpr("\\d", input)),
         regmatches(input, regexpr("\\d(?=[^\\d]*$)", input, perl = TRUE))) |>
    as.numeric() |>
    sum()
}

testthat::expect_equal(
  extract_number(test),
  142
)
#Answer Part 1: 54597
extract_number(input)