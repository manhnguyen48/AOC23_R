input <- read_file("day_15/00_input/input.txt")
test <- read_file("day_15/00_input/test.txt")

parse_input <- function(input_text) {
  strsplit(gsub("\\n|\\r", "", input_text), ",")[[1]]
}
#Function to hash a string based on the ASCII character number
hash <- function(string, M=17, D=256) {
  Reduce(\(x, y) { ((x + y) * M) %% D}, utf8ToInt(string), init = 0)
}
parse_input(test) |> vapply(hash,numeric(1)) |> sum() == 1320
#Answer Part 1: 511498
parse_input(input) |> vapply(hash, numeric(1)) |> sum()
