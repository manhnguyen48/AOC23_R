parse_input <- function(input_text) {
  strsplit(input_text, "") |>
    stringi::stri_list2matrix(byrow=TRUE)
}

dfs_traverse <- function(mtx, starting) {
  stack <- collections::stack()
  stack$push(starting)
  seen <- c()
  seen_hash <- hash::hash()
  seen <- c(seen, paste0(starting, collapse = "_"))
  seen_hash[[paste0(starting, collapse = "_")]] <- TRUE
  while (stack$size() > 0) {
    node <- stack$pop()
    x <- node[1]
    y <- node[2]
    s <- mtx[x, y] #Symbol
    #Going up
    if (x > 1 &&
        s %in% c("|", "L", "J", "S") &&
        mtx[x - 1, y] %in% c("|", "7", "F") &&
        is.null(seen_hash[[paste0(x-1,"_",y)]])
    ) {
      seen <- c(seen, paste0(x - 1, "_", y))
      seen_hash[[paste0(x - 1, "_", y)]] <- TRUE
      stack$push(c(x - 1, y))
    }
    #Going down
    if (x < nrow(mtx) &&
        s %in% c("|", "7", "F", "S") &&
        mtx[x + 1, y] %in% c("|", "L", "J") &&
        is.null(seen_hash[[paste0(x + 1, "_", y)]])
    ) {
      seen <- c(seen, paste0(x + 1, "_", y))
      seen_hash[[paste0(x + 1, "_", y)]] <- TRUE
      stack$push(c(x + 1, y))
    }
    #Going left
    if (y > 1 &&
        s %in% c("-", "J", "7", "S") &&
        mtx[x, y - 1] %in% c("-", "L", "F") &&
        is.null(seen_hash[[paste0(x, "_", y - 1)]])
    ) {
      seen <- c(seen, paste0(x , "_", y-1))
      seen_hash[[paste0(x , "_", y-1)]] <- TRUE
      stack$push(c(x, y - 1))
    }
    #Going right
    if (y < ncol(mtx) &&
        s %in% c("-", "L", "F", "S") &&
        mtx[x, y + 1] %in% c("-", "J", "7") &&
        is.null(seen_hash[[paste0(x, "_", y + 1)]])
    ) {
      seen <- c(seen, paste0(x , "_", y+1))
      seen_hash[[paste0(x , "_", y+1)]] <- TRUE
      stack$push(c(x, y + 1))
    }
  }
  return(seen)
}

