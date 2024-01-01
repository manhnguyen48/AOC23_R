input <- readLines("day_23/00_input/input.txt")
test <- readLines("day_23/00_input/test.txt")

parse_input <- function(input_text) {
  strsplit(input_text, "") |>
    stringi::stri_list2matrix(byrow=TRUE)
}

parsed_test <- parse_input(test)
parsed_input <- parse_input(input)

#Function to find all the points with at least 3 neighbours
poi_extract <- function(mtx) {
  coords <- which(mtx != "#", arr.ind = TRUE)
  max_rw <- nrow(mtx)
  max_cl <- ncol(mtx)
  start <- coords[coords[,"row"] == 1,] |> as.integer()
  end <- coords[coords[,"row"] == max_rw, ] |> as.integer()
  poi <- list(start, end)
  dirs <- list(
    "^" = c(-1, 0),
    "v" = c(1, 0),
    ">" = c(0, 1),
    "<" = c(0, -1)
  )
  for (pair in 1:nrow(coords)) {
    rw <- as.integer(coords[pair, "row"])
    cl <- as.integer(coords[pair, "col"])
    all_nbrs <- 0
    for (i in seq_along(dirs)) {
      dir <- dirs[[i]]
      nrw <- rw + dir[1]
      ncl <- cl + dir[2]
      #Check if we're in bound or haven't hit a bush
      if (nrw < 1 ||
          nrw > max_rw ||
          ncl < 1 ||
          ncl > max_cl ||
          mtx[nrw, ncl] == "#") {
        next
      }
      all_nbrs <- all_nbrs + 1
    }
    if (all_nbrs >= 3) {
      poi <- append(poi, list(c(rw, cl)))
    }
  }
  return(poi)
}
#Depth first search out from the point of interest
edge_contract <- function(mtx, poi) {
  edge_list <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(edge_list) <- c("From", "To", "Weight")
  dirs <- list(
    "^" = list(c(-1, 0)),
    "v" = list(c(1, 0)),
    "<" = list(c(0, -1)),
    ">" = list(c(0, 1)),
    "." = list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  )
  for (pt in poi) {
    sr <- pt[1]
    sc <- pt[2]
    q <- collections::stack()
    q$push(c(sr, sc, 0))
    seen <- collections::dict()
    seen$set(paste0(pt, collapse = "_"), TRUE)
    while (q$size() > 0) {
      tile <- q$pop()
      x <- tile[1]
      y <- tile[2]
      n <- tile[3]
      if (n != 0 && (list(as.integer(c(x, y))) %in% poi)) {
        edge_list <- rbind(edge_list,
                           data.frame(
                             From = paste(c(sr, sc), collapse = "_"),
                             To = paste(c(x, y), collapse = "_"),
                             Weight = n
                           ))
        next
      }
      for (dxy in dirs[[mtx[x, y]]]) {
        nx <- x + dxy[1]
        ny <- y + dxy[2]
        if (nx >= 1 &&
            nx <= nrow(mtx) &&
            ny >= 1 &&
            ny <= ncol(mtx) &&
            mtx[nx, ny] != "#" &&
            !seen$has(paste0(c(nx, ny), collapse = "_"))) {
          q$push(c(nx, ny, n + 1))
          seen$set(paste0(c(nx, ny), collapse = "_"), TRUE)
        }
      }
    }
  }
  return(edge_list)
}

longest_path <- function(mtx) {
  #Find start and end coordinate and contract the graph
  start <- c(1, which(mtx[1, ] == ".", TRUE))
  end <- c(nrow(mtx), which(mtx[nrow(mtx), ] == ".", TRUE))
  gdf <- edge_contract(mtx, poi_extract(mtx))

  #Recursive function to find longest path
  seen <- collections::dict()
  dfs <- function(pt) {
    #Exit condition if we've arrived at the end
    if (identical(as.integer(pt), end)) { return(0) }
    pt_name <- paste(pt, collapse = "_")
    m <- -Inf
    seen$set(pt_name, TRUE)
    for (nx in gdf$To[gdf$From == pt_name]) {
      if (!seen$has(nx)) {
        m <- max(m, dfs(as.numeric(strsplit(nx, "_")[[1]])) +
                   gdf$Weight[gdf$From == pt_name & gdf$To == nx])
      }
    }
    seen$remove(pt_name)
    return(m)
  }
  dfs(start)
}

longest_path(parsed_test) == 94
#Answer Part 1: 2238
longest_path(parsed_input)
