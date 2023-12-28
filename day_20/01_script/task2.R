input <- readLines("day_20/00_input/input.txt")
test <- readLines("day_20/00_input/test.txt")
source("day_20/01_script/custom_module.R")

parse_input <- function(input_text) {
  tmp <- strsplit(input_text, " -> ")
  broadcast_targets <- c()
  out  <- list()
  for (node in tmp) {
    targets <- unlist(strsplit(node[[2]], ", "))
    if (node[[1]] == "broadcaster") {
      broadcast_targets <- targets
    } else {
      mod_type <- substr(node[[1]], 1,1)
      mod_name <- substr(node[[1]], 2, nchar(node[[1]]))
      out[[mod_name]] <- Node(mod_name, mod_type, targets)
    }
  }
  #We need another loop to update the states of the & modules
  #check every node and if it's targeting a conjunction node then update
  #the state of those nodes, default to low pulse - FALSE
  for (node in out) {
    for (target_name in node[["targets"]]) {
      if (target_name %in% names(out) &&
          out[[target_name]][["type"]] == "&") {
        out[[target_name]][["in_value"]] <-
          c(out[[target_name]][["in_value"]],
            setNames(FALSE, nm = node[["name"]]))
      }
    }
  }
  return(list("broadcast_targets" = broadcast_targets,
              "nodes" = out))
}
parsed_test <- parse_input(test)
parsed_input <- parse_input(input)


find_cycle_length <- function(data) {
  #There should be only one node feeding directly into rx and it should be a
  #conjunction node;
  rx_direct_feed <- Find(\(x) {"rx" %in% x[["targets"]]}, data$nodes)[["name"]]
  #Find out which nodes are 2 steps away from the rx node;
  rx_indirect_feed <- Filter(\(x) {rx_direct_feed %in% x[["targets"]]}, data$nodes) |>
    names()
  #Because the node feeding directly we just need to see how many cycles does it take
  #each indirect nodes to turn to low
  cycles <- setNames(rep(NA, length(rx_indirect_feed)), rx_indirect_feed)
  seen <- setNames(rep(0, length(rx_indirect_feed)), rx_indirect_feed)
  presses <- 0

  while (TRUE) {
    presses <- presses + 1
    #From the button
    queue <- collections::deque()
    #Initial pulse
    for (target in data[["broadcast_targets"]]) {
      queue$push(list("broadcaster", target, c("broadcaster"=FALSE)))
    }
    while(queue$size() > 0) {
      next_pulse <- queue$popleft()
      origin_node <- next_pulse[[1]]
      target_node <- next_pulse[[2]]
      pulse_value <- next_pulse[[3]]
      #Skip if we don't have a logic node
      if (!(target_node %in% names(data[["nodes"]])) ) {next}
      #If we have high pulse and a target node of %, skip the pulse entirely
      if (data$nodes[[target_node]]$type == "%" & pulse_value) {
        next
      }
      if (target_node == rx_direct_feed && pulse_value == TRUE) {
        seen[[origin_node]] <- seen[[origin_node]] + 1
        if (is.na(cycles[[origin_node]])) {
          cycles[[origin_node]] <- presses }
        if (all(!is.na(cycles))) {
          return(Reduce(pracma::Lcm, cycles))
        }
      }
      #Performing the pulse here
      data[["nodes"]][[target_node]]$pulse(pulse_value)
      #Appending values to the queue
      new_origin <- data[["nodes"]][[target_node]][["name"]]
      new_state <- data[["nodes"]][[target_node]][["state"]]
      for (target in data[["nodes"]][[target_node]][["targets"]]) {
        queue$push(list(new_origin, target,
                        setNames(new_state, new_origin)))
      }
    }
  }
}

#Answer Part 2: 229_215_609_826_339
find_cycle_length(parsed_input)
