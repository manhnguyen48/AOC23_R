input <- readLines("day_09/00_input/input.txt")
test <- readLines("day_09/00_input/test.txt")

parse_input <- function(input_text) {
  lapply(input_text,
         \(x) strsplit(x, "( )+")[[1]] |>
           as.numeric())
}

parsed_input <- parse_input(input)
parsed_test <- parse_input(test)

diff_predict <- function(x) {
  diff_vec <- list(x)
  current_diff <- diff(x)
  while (!all(current_diff == 0)) {
    diff_vec <- append(diff_vec, list(current_diff), after = 0)
    current_diff <- diff(current_diff)
  }
  #Add the all 0
  diff_vec <- append(diff_vec, list(rep(0, length(current_diff))), after = 0)
  out <- Reduce("+",sapply(diff_vec, \(x) x[length(x)]))
  return(out)
}

testthat::expect_equal(
  vapply(parsed_test,diff_predict,numeric(1)) |> sum(),
  114
)
#Answer Part 1: 1798691765
vapply(parsed_input,diff_predict,numeric(1)) |> sum()