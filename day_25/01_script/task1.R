input <- readLines("day_25/00_input/input.txt")
test <- readLines("day_25/00_input/test.txt")

library(igraph)

#Construct an undirected graph from the inputs
parse_input <- function(input_text) {
  out <- strsplit(input_text, ": ") |>
    lapply(\(line) {
      rhs <- strsplit(line[[2]], " ")[[1]]
      data.frame(from = rep(line[[1]], length(rhs)),
                 to = rhs)
    })
  igraph::graph_from_data_frame(do.call(rbind, out),
                                directed = FALSE)
}

g_test <- parse_input(test)
g_input <- parse_input(input)

cut_graph <- function(g) {
  cutting <- igraph::min_cut(g, value.only = FALSE)
  length(cutting$partition1) * length(cutting$partition2)
}

cut_graph(g_test) == 54
#Answer: 556467
cut_graph(g_input)
