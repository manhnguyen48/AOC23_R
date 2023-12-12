input <- readLines("day_12/00_input/input.txt")
test <- readLines("day_12/00_input/test.txt")

#Different parsing as we need to unroll the inputs
parse_input <- function(input_text) {
  tmp <- strsplit(input_text, " ")
  gears <- sapply(tmp, \(x) strsplit(x[[1]],"")[[1]] |>
                    paste0(collapse = "") |>
                    rep(5) |>
                    paste0(collapse = "?"))
  criteria <- sapply(tmp, \(x) strsplit(x[[2]], ",")[[1]] |>
                       as.numeric()) |>
    sapply(\(x) rep(x, 5))
  return(list(gears = gears, criteria = criteria))
}

parsed_test <- parse_input(test)
parsed_input <- parse_input(input)

#I copied this from the solution from Andrea Barghetti
#I saw this version from hyper-neutrino as well but couldn't translate Python index
#to R index in some parts
#https://github.com/AndreaBarghetti/AdventOfCode/blob/main/AoC2023/Day12/

count_seqs_dp <- function(sequence, counts, cache) {
  key <- paste(sequence, toString(counts))
  if (exists(key, envir = cache)) {
    return(get(key, envir = cache))
  }
  if (sequence == "") {
    return(ifelse(length(counts) == 0, 1, 0))
  }
  if (length(counts) == 0) {
    return(ifelse(grepl("#", sequence), 0, 1))
  }
  result <- 0
  if (str_sub(sequence, 1, 1) %in% c(".", "?")) {
    result <-
      result + count_seqs_dp(str_sub(sequence, 2, -1), counts, cache)
  }
  if (str_sub(sequence, 1, 1) %in% c("#", "?")) {
    if (counts[1] <= nchar(sequence) &&
        !grepl("\\.", str_sub(sequence, 1, counts[1])) &&
        (counts[1] == nchar(sequence) ||
         str_sub(sequence, counts[1] + 1, counts[1] + 1) != "#")) {
      result <-
        result + count_seqs_dp(str_sub(sequence, counts[1] + 2,
                                         nchar(sequence)),
                               counts[-1], cache)
    }
  }
  cache[[key]] <- result
  return(result)
}

options(scipen = 999)
run_counts <- function(input_list) {
  cache <- rlang::env()
  purrr::map2_dbl(
    input_list$gears,
    input_list$criteria,
    \(x,y) count_seqs_dp(x, y, cache)
  ) |> sum()
}


run_counts(parsed_test) == 525152

run_counts(parsed_input)