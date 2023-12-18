input <- readLines("day_18/00_input/input.txt")
test <- readLines("day_18/00_input/test.txt")

parse_input <- function(input_text) {
  strsplit(input_text, " ") |>
    lapply(\(x) {
      d <- x[[1]]
      d <- switch(
        d,
        R = c(0, 1),
        L = c(0, -1),
        U = c(-1, 0),
        D = c(1, 0)
      )
      steps <- as.numeric(x[[2]])
      return(list(
        "d" = d,
        "steps" = steps
      ))
    })
}

parsed_input <- parse_input(input)
parsed_test <- parse_input(test)

dig_trench <- function(instructions) {
  start <- c(0, 0)
  trench <- list(start)
  for (turn in instructions) {
    d <- turn[["d"]]
    steps <- turn[["steps"]]
    for (s in 1:steps) {
      current <- trench[[length(trench)]]
      new_coords <- current + d
      trench <- append(trench, list(current + d))
    }
  }
  out <- unlist(trench[-1]) |>
    matrix(ncol = 2, byrow= TRUE, dimnames = list(NULL, c("x", "y")))
  return(out)
}

shoelace_pick <- function(mtx) {
  b <- nrow(mtx)
  #Append first row to complete the loop
  mtx <- rbind(mtx, mtx[1,])
  #Shoelace formula from: https://en.wikipedia.org/wiki/Shoelace_formula
  area <- 0.5 * abs(sum(mtx[-nrow(mtx), 1] * mtx[-1, 2] - mtx[-1, 1] * mtx[-nrow(mtx), 2]))
  #Pick's theorem adjustment because the boundary points also count as area
  #and it might curves inwards https://en.wikipedia.org/wiki/Pick%27s_theorem
  inside <- area - b / 2 + 1
  inside_area <- b + inside
  return(inside_area)
}

dig_trench(parsed_test) |> shoelace_pick() == 62
#Answer Part 1: 38188
dig_trench(parsed_input) |> shoelace_pick()

