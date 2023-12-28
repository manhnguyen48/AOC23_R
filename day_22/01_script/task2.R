input <- readLines("day_22/00_input/input.txt")
test <- readLines("day_22/00_input/test.txt")

parse_input <- function(input_text) {
  out <- strsplit(input_text, "~") |>
    lapply(\(x) strsplit(x, ",")) |>
    unlist() |>  as.integer()  |>
    matrix(ncol = 6,
           byrow = TRUE,
           dimnames = list(NULL,
                           c("x1", "y1", "z1",
                             "x2", "y2", "z2")))
  out <- out[order(out[, "z1"]), ]
}
parsed_test <- parse_input(test)
parsed_input <- parse_input(input)
#Utility function to check if to brick overlap on x and y dimensions
overlap_xy <- function(b1, b2) {
  overlap_x <- max(b1["x1"], b2["x1"]) <= min(b1["x2"], b2["x2"])
  overlap_y <- max(b1["y1"], b2["y1"]) <= min(b1["y2"], b2["y2"])
  return(overlap_x && overlap_y)
}
fall_down <- function(mtx) {
  for (i in 2:nrow(mtx)) {
    bottom_z <- 1
    cur_brick <- mtx[i, ]
    below_bricks <- mtx[1:(i-1),,drop = FALSE]
    for (j in 1:nrow(below_bricks)) {
      if (overlap_xy(below_bricks[j,], cur_brick)) {
        bottom_z <- max(bottom_z, below_bricks[j, "z2"] + 1)
      }
    }
    mtx[i, "z2"] <- mtx[i, "z2"] - (mtx[i, "z1"] - bottom_z)
    mtx[i, "z1"] <- bottom_z
  }
  mtx <- mtx[order(mtx[, "z1"]), ]
  return(mtx)
}
#Same as before but we need to return the list of support and depends for
#calculations later
check_support <- function(mtx) {
  #First fall the bricks down
  stacked_mtx <- fall_down(mtx)
  supports <- vector("list", nrow(mtx))
  depends <- vector("list", nrow(mtx))
  for (j in 1:nrow(stacked_mtx)) {
    for (i in 1:(j - 1)) {
      if (length(stacked_mtx[i,]) > 0) {
        if (overlap_xy(stacked_mtx[j,], stacked_mtx[i,]) &&
            (stacked_mtx[j, "z1"] == stacked_mtx[i, "z2"] + 1)) {
          supports[[i]] <- c(supports[[i]], j)
          depends[[j]] <- c(depends[[j]], i)
        }
      }
    }
  }
  return(list("supports" = supports,
              "depends" = depends))
}
sup_dep_test <- check_support(parsed_test)
sup_dep_input <- check_support(parsed_input)

chain_reaction <- function(mtx, supports, depends) {
  total <- 0
  #Looping through each bricks and breadth first search of how many bricks
  #would be broken given we know the dependency graph
  for (i in 1:nrow(mtx)) {
    #Keep track of how many bricks would be broken
    broken <- vector("logical", nrow(mtx))
    queue <- collections::deque(i)
    while(queue$size() > 0) {
      j <- queue$pop()
      broken[j] <- TRUE
      deps <- supports[[j]]
      for (d in deps) {
        sups <- depends[[d]]
        if (all(broken[sups])) {
          queue$push(d)
        }
      }
    }
    broken[i] <- FALSE
    total <-  total + sum(broken)
  }
return(total)
}

chain_reaction(parsed_test, sup_dep_test$supports, sup_dep_test$depends) == 7
#Answer Part 2: 76158
chain_reaction(parsed_input, sup_dep_input$supports, sup_dep_input$depends)
