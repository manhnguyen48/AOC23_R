input <- readLines("day_14/00_input/input.txt")
test <- readLines("day_14/00_input/test.txt")

parse_input <- function(input_text) {
  strsplit(input_text, "") |>
    stringi::stri_list2matrix(byrow = TRUE)
}
parsed_input <- parse_input(input)
parsed_test <- parse_input(test)

#Split the array by #, then sort descending the symbols so O would come first
#then put it back in an array
split_and_sort <- function(x) {
  out <-
    stringi::stri_split(paste0(x, collapse = ""), regex = "#")[[1]] |>
    vapply(
      \(x) {
        if (x!="") {
          strsplit(x, "")[[1]] |>
            sort(decreasing = TRUE) |>
            paste0(collapse = "")
        } else {
          return(x)
        }
      }, character(1), USE.NAMES = FALSE
    ) |>
    paste0(collapse = "#") |>
    strsplit("")
  return(out[[1]])
}
#tilting the matrix north side
tilt <- function(mtx) {
  for (j in 1:ncol(mtx)) { mtx[, j] <- split_and_sort(mtx[, j]) }
  return(mtx)
}
count_load <- function(mtx) {
  #Get the row index of all O then subtract them from the number of row + 1
  #to get the distance to bottom
  sum(nrow(mtx) + 1 - which(mtx == "O", arr.ind = TRUE)[, "row"])
}
#Rotate a matrix 90 degree clockwise, West now face North
rotate <- function(mtx) {
  t(mtx[nrow(mtx):1,,drop=FALSE])
}
#Complete 1 cycle
cycle <- function(mtx) {
  rotated <- mtx
  for (i in 1:4) { rotated <- rotate(tilt(rotated)) }
  return(rotated)
}

tumble_cycle <- function(mtx, max_iter = 10 ^ 9) {
  seen_states <- rlang::env()
  unique_states <- c()
  cur_state <- mtx
  i <-  0
  #keep cycling until we've run into a the same state
  while (TRUE) {
    i <- i + 1
    cur_state <- cycle(cur_state)
    mtx_str <- paste0(cur_state, collapse = "")
    #if we've seen this state of the matrix then stop cycling
    if (exists(mtx_str, envir = seen_states)) {
      break
    }
    #Otherwise update our set of seen states
    seen_states[[mtx_str]] <- i
    #Recording the unique new state so we can index later
    unique_states <- c(unique_states, mtx_str)
  }
  #Don't need to keep running till the max cycle number as we know states
  #will repeat anyway so just need to take the remainder of the cycle
  start_of_cycle <- which(unique_states == mtx_str)
  cycle_length <- i - start_of_cycle
  final_state_id <-
    start_of_cycle + ((max_iter - start_of_cycle) %% cycle_length)
  #Putting back our matrix
  strsplit(unique_states[final_state_id], "")[[1]] |>
    matrix(nrow = nrow(mtx), ncol = ncol(mtx))
}

count_load(tumble_cycle(parsed_test)) == 64
#Answer Part 2: 104619
count_load(tumble_cycle(parsed_input))