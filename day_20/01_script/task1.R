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
        out[[target_name]][["in_value"]] <- c(out[[target_name]][["in_value"]],
                                           setNames(FALSE, nm = node[["name"]]))
      }
    }
  }
  return(list("broadcast_targets" = broadcast_targets,
              "nodes" = out))
}
#Use a queue to handle pulses order
send_pulses <- function(data, presses = 1000) {
  pulses <- c()
  for (i in 1:presses) {
    #From the button
    pulses <- c(pulses, FALSE)
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
      pulses <- c(pulses, unname(pulse_value))
      #Skip if we don't have a logic node
      if (!(target_node %in% names(data[["nodes"]])) ) {next}
      #If we have high pulse and a target node of %, skip the pulse entirely
      if (data$nodes[[target_node]]$type == "%" & pulse_value) {
        next
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
  return(pulses)
}

parsed_test <- parse_input(test)
parsed_input <- parse_input(input)

send_pulses(parsed_test) |> table() |> prod() == 11687500
#Answer Part 1: 866435264
send_pulses(parsed_input) |> table() |> prod()

