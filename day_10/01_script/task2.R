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

#Manually reorder the first few pipesso we would go in 1 direction
tmp <- dfs_traverse(parsed_test, c(2,2))
shoelace(tmp[c(2, 1, 3:length(tmp))]) == 4
#Answer Part 2: 423
tmp2 <- dfs_traverse(parsed_input, c(77,110))
shoelace(tmp2[c(2,1,3:length(tmp2))])
