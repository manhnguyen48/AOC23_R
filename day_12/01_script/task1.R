input <- readLines("day_12/00_input/input.txt")
test <- readLines("day_12/00_input/test.txt")

parse_input <- function(input_text) {
  tmp <- strsplit(input_text, " ")
  gears <- sapply(tmp, \(x) strsplit(stringr::str_replace_all(x[[1]],
                                                              "\\.+", "\\."),
                                     "")[[1]] )
  criteria <- sapply(tmp, \(x) strsplit(x[[2]], ",")[[1]] |>
                     as.numeric())
  return(list(gears = gears, criteria = criteria))
}

parsed_test <- parse_input(test)
parsed_input <- parse_input(input)

#Brute force to flip every switch, the encoding order is actually just
#running lengths encoding
check_config <- function(line, order) {
  total <-  sum(order)
  total_broken <-  sum(line == "#")
  current_unknown <-  which(line == "?")
  perm_unknown <-  gtools::permutations(n=2, v=c("#", "."),
                                        r = length(current_unknown),
                                        repeats.allowed = TRUE)
  valid_combo <- rowSums(perm_unknown == "#") == (total-total_broken)
  #We know how many broken left so only going to test relevant sets
  perm_unknown = perm_unknown[valid_combo,,drop = FALSE]
  count_valid = 0
  #Replace all ? with our permutations and test the rle encoding
  for (i in 1:nrow(perm_unknown)) {
    tmp <- line
    tmp[current_unknown] <- perm_unknown[i, ]
    encoded <- purler::rlenc(tmp)
    translated <- encoded$lengths[which(encoded$values=="#")]
    if (length(translated) == length(order)) {
      if (all(translated == order)) {
        count_valid = count_valid + 1
      }
    }
  }
  return(count_valid)
}

sum(purrr::map2_dbl(
  parsed_test$gears,
  parsed_test$criteria,
  \(x,y) check_config(x,y)
)) == 21

#Answer Part 1: 7260
sum(purrr::map2_dbl(
  parsed_input$gears,
  parsed_input$criteria,
  \(x,y) check_config(x,y)
))