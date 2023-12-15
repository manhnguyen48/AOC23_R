input <- read_file("day_15/00_input/input.txt")
test <- read_file("day_15/00_input/test.txt")
#Changed parsing function to return named character
parse_input <- function(input_text) {
  strsplit(gsub("\\n|\\r", "", input_text), ",")[[1]] |>
    lapply(\(x) {
      name <- stringi::stri_extract(x, regex = "[:alpha:]+")
      value <- stringi::stri_extract(x, regex = "[:digit:]+")
      setNames(as.numeric(value), nm = name)
    }) |> unlist()
}
parsed_test <- parse_input(test)
parsed_input <- parse_input(input)
hash <- function(string, M=17, D=256) {
  Reduce(\(x, y) { ((x + y) * M) %% D}, utf8ToInt(string), init = 0)
}

stack_lens <- function(lens) {
  #Initiate a hash table
  h1 <- utils::hashtab(size = 256)
  for (i in seq_along(lens)) {
    label <- names(lens[i])
    key <- toString(hash(label))
    cur_val <- lens[[i]]
    hash_value <- as.list(utils::gethash(h1, key))
    #If the value is NA then we remove the label from the list, otherwise update it
    if (is.na(cur_val)) {
      hash_value[[label]] <- NULL
    } else {
      hash_value[[label]] <- cur_val
    }
    utils::sethash(h1, key, hash_value)
  }
  #Calculating the focusing power of all lenses, order of boxes doesn't matter
  #Also we can skip box without any lenses
  total <-  0
  utils::maphash(h1, \(k, v) {
    if (length(v)>0) {
      values <- as.numeric(v)
      indices <- 1:length(v)
      box_value <- as.numeric(k) + 1
      total <<- total + sum(box_value * indices * values)
    }
  })
  return(total)
}

stack_lens(parsed_test) == 145
#Answer Part 2: 284674
stack_lens(parsed_input)