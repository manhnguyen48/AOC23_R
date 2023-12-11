input <- readLines("day_11/00_input/input.txt")
test <- readLines("day_11/00_input/test.txt")

parse_input <- function(input_text) {
  width <- vapply(input_text, nchar, numeric(1)) |> unique()
  height <- length(input_text)
  mtx <- vapply(input_text, \(x) strsplit(x, "")[[1]], character(width),
         USE.NAMES = FALSE) |>
    as.vector() |>
    matrix(nrow = height, ncol= width, byrow = TRUE,
           dimnames = list(c(1:height), c(1:width)))
  return(mtx)
}
input_mtx <- parse_input(input)
test_mtx <- parse_input(test)

#Function to convert galaxy coordinate to Cartesian plane, accounting for the
#expansion factor
expand_matrix <- function(mtx, expansion_factor=2) {
  #Empty matrix to collect the Cartesian coordinate of galaxy
  coords <- matrix(nrow = sum(mtx == "#"), ncol = 2)
  empty_rows <- rownames(mtx)[apply(mtx, 1, \(x) all(x=="."))] |> as.numeric()
  empty_cols <- colnames(mtx)[apply(mtx, 2, \(x) all(x=="."))] |> as.numeric()
  x <- as.numeric(rownames(mtx))
  y <- as.numeric(colnames(mtx))
  #Add expansion factor to index
  for (r in empty_rows) {
    x[r:length(x)] <- x[r:length(x)] + expansion_factor - 1
  }
  for (c in empty_cols) {
    y[c:length(y)] <- y[c:length(y)] + expansion_factor - 1
  }
  out <- tibble::as_tibble(which(mtx=="#", arr.ind = T)) |>
    dplyr::mutate(x = x[row], y = y[col], .keep = "unused")
  return(out)
}

testthat::expect_equal(
  expand_matrix(test_mtx, 2) |>
    dist(method = "manhattan") |>
    sum(),
  374
)
#Answer Part 1: 9370588
expand_matrix(input_mtx, 2) |>
  dist(method = "manhattan") |>
  sum()