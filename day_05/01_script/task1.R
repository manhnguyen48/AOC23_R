input <- readr::read_file("day_05/00_input/input.txt")
test <- readr::read_file("day_05/00_input/test.txt")

#Parsing seed as list of integers, and maps as matrices
parse_input <- function(input_text) {
  tmp <- strsplit(input_text, "(\r)?\n(\r)?\n")[[1]]
  seeds <- stringi::stri_extract(tmp[1], regex="(?<=seeds\\: ).*") |>
    strsplit(" ") |> unlist() |>  as.numeric()
  maps <- list()
  for (m in  tmp[2:length(tmp)]) {
    splitted <- strsplit(m, ":(\r)?\n")[[1]]
    map_name <- splitted[1]
    map_values <- strsplit(splitted[2], "(\r)?\n")[[1]] |>
      strsplit(" ")|>
      stringi::stri_list2matrix(byrow=TRUE) |>
      apply(2, as.numeric)
    dimnames(map_values) <- list(NULL, c("to", "from", "range"))
    #Order by the range of from values
    maps[[map_name]] <- map_values[order(map_values[, "from"]), ]
  }
  return(list("seeds"=seeds, "maps"=maps))
}
parsed_test <- parse_input(test)
parsed_input <- parse_input(input)

convert_single <- function(seeds, maps) {
  cur_seeds <- seeds
  for (map in maps) {
    new_seeds <- c()
    for (seed in cur_seeds) {
      matches <- c()
      apply(map, 1, \(x) {
        #Check each range if we're in bound
        if (dplyr::between(seed, x[["from"]], x[["from"]] + x[["range"]] - 1)) {
          matches <<- c(matches, seed - x[["from"]] + x[["to"]])
        }
      })
      #If we don't find any matches then map to itself
      new_seeds <- if (length(matches)==0) {
        c(new_seeds, seed)
      } else {
        c(new_seeds, matches)
      }
    }
    cur_seeds <- new_seeds
  }
  return(new_seeds)
}

convert_single(parsed_test$seeds, parsed_test$maps) |> min() == 35
#Answer Part 1: 226172555
convert_single(parsed_input$seeds, parsed_input$maps) |> min()
