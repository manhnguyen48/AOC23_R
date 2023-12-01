
require(stringr)
require(stringi)
require(purrr)

input <- readLines("./day_01/00_input/input.txt")

tidyverse_1 <- function(input) {
  stringr::str_extract_all(input, "[:digit:]") |>
    purrr::map_dbl(\(x) paste0(x[1], x[length(x)], collapse = "") |>
                     as.numeric()) |>
    sum()
}

tidyverse_2 <- function(input) {
  paste0(
    stringi::stri_extract_first(input, regex = "[:digit:]"),
    stringi::stri_extract_last(input, regex = "[:digit:]")
  ) |>
    as.numeric() |>
    sum()
}


baseR_way <- function(input) {
  paste0(regmatches(input, regexpr("\\d", input)),
         regmatches(input, regexpr("\\d(?=[^\\d]*$)", input, perl = TRUE))) |>
    as.numeric() |>
    sum()
}


tidyverse_1(input)

tidyverse_2(input)

baseR_way(input)


microbenchmark::microbenchmark(tidyverse_1(input),
                               tidyverse_2(input),
                               baseR_way(input),
                               times = 1000)
