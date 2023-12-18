input <- readr::read_file("day_05/00_input/input.txt")
test <- readr::read_file("day_05/00_input/test.txt")

#Changing the parsing of seeds so it's a 2 column matrix
parse_input <- function(input_text) {
  tmp <- strsplit(input_text, "(\r)?\n(\r)?\n")[[1]]
  seeds <- stringi::stri_extract(tmp[1], regex="(?<=seeds\\: ).*") |>
    strsplit(" ") |> unlist() |>  as.numeric()
  starts <- seeds[seq(1, length(seeds), 2)]
  ends <- starts + seeds[seq(2, length(seeds), 2)] - 1
  seeds <- mapply(c, starts,ends, SIMPLIFY = FALSE)
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

convert_ranges <- function(seeds_list, maps) {
  #using queue here as it's easier to pop off one thing at a time
  cur_ranges <- collections::queue(seeds_list)
  for (map in maps) {
    new_ranges <- collections::queue()
    while (cur_ranges$size() > 0) {
      cur_range <- cur_ranges$pop()
      s_start <- cur_range[[1]]
      s_end <- cur_range[[2]]
      #Initiate a list in case we don't find any matches
      matches <- list()
      for (i in 1:nrow(map)) {
        overlap_start <- max(s_start, map[i, "from"])
        overlap_end <- min(s_end, map[i, "from"] + map[i, "range"])
        #We could only find matches if the overlap start is smaller than overlap end
        if (overlap_start < overlap_end) {
          #Only map the ends here
          new_range <- c(overlap_start - map[i, "from"] + map[i, "to"],
                         overlap_end - map[i, "from"] + map[i, "to"])
          matches <- append(matches, list(new_range))
          if (overlap_start > s_start) {
            #Update our current queue with the left hand side section
            cur_ranges$push(c(s_start, overlap_start))
          }
          if (s_end > overlap_end) {
            #Or the right hand side part
            cur_ranges$push(c(overlap_end, s_end))
          }
          break
        }
      }
      #If we don't find any matches then the range map to itself
      if (length(matches) == 0) {
        new_ranges$push(c(s_start, s_end))
      } else {
        lapply(matches, \(x) new_ranges$push(x))
      }
    }
    cur_ranges <- new_ranges
  }
  out <- new_ranges$as_list() |> unlist() |> unname()
  return(out)
}

convert_ranges(parsed_test$seeds, parsed_test$maps) |> min() == 46
#Answer Part 2: 47909639
convert_ranges(parsed_input$seeds, parsed_input$maps) |> min()
