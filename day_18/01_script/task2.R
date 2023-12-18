input <- readLines("day_18/00_input/input.txt")
test <- readLines("day_18/00_input/test.txt")

parse_input <- function(input_text) {
  strsplit(input_text, " ") |>
    lapply(\(x) {
      hexcode <- gsub("\\(|\\)", "", x[[3]]) |>
        strsplit("") |> unlist()
      d <- switch(hexcode[7],
                  "0" = c(0,1),
                  "1" = c(1,0),
                  "2" = c(0,-1),
                  "3" = c(-1,0))
      steps <- paste0(hexcode[2:6],collapse = "") |>
        as.hexmode() |>
        as.numeric()
      return(list(
        "d" = d,
        "steps" = steps
      ))
    })
}
parsed_input <- parse_input(input)
parsed_test <- parse_input(test)
#Modified function to avoid going through all points manually
dig_trench_large <- function(instructions) {
  start <- c(0, 0)
  trench <- list(start)
  #Keep track of how many points we're adding
  num_points <- 0
  for (turn in instructions) {
    d <- turn[["d"]]
    steps <- turn[["steps"]]
    #For shoelace we don't really care about the edges, just the points
    current <- trench[[length(trench)]]
    new_coords <- current + d * steps
    num_points <- num_points + steps
    trench <- append(trench, list(new_coords))
  }
  out <- unlist(trench[-1]) |>
    matrix(ncol = 2, byrow= TRUE, dimnames = list(NULL, c("x", "y")))
  return(list("mtx" = out, "num_points" = num_points))
}

shoelace_pick <- function(res) {
  mtx <- res[["mtx"]]
  b <- res[["num_points"]]
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

dig_trench_large(parsed_test) |> shoelace_pick() == 952408144115
#Answer Part 2: 93325849869340
dig_trench_large(parsed_input) |> shoelace_pick()
