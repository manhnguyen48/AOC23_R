source("day_10/01_script/shared_function.R")
input <- readLines("day_10/00_input/input.txt")
test <- readLines("day_10/00_input/test2.txt")


parsed_test <- parse_input(test)
parsed_input <- parse_input(input)

shoelace <- function(ordered_names) {
  mtx <- strsplit(ordered_names, "_") |>
    unlist() |>
    as.numeric() |>
    matrix(ncol = 2, byrow=TRUE)
  #Append first row to complete the loop
  b <- nrow(mtx)
  mtx <- rbind(mtx, mtx[1,])
  #Shoelace formula from: https://en.wikipedia.org/wiki/Shoelace_formula
  area <- 0.5 * abs(sum(mtx[-nrow(mtx), 1] * mtx[-1, 2] - mtx[-1, 1] * mtx[-nrow(mtx), 2]))
  inside <- area - b / 2 + 1
  return(inside)
}

#Manually reorder the first few pipes so we would go in 1 direction
dfs_traverse(parsed_test, as.numeric(which(parsed_test=="S", TRUE))) |>
  shoelace() == 4
#Answer Part 2: 423
dfs_traverse(parsed_input, as.numeric(which(parsed_input=="S", TRUE))) |>
  shoelace()
