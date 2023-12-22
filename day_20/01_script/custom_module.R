#Create a custom class of the nodes,
#representing the pulses as TRUE for high, FALSE for low
#Using named character to represent state of & modules
Node <- setRefClass("Node",
  fields = list(
    "name" = "character",
    "type" = "character",
    "targets" = "character",
    "in_value" = "logical",
    "state" = "logical"
  ),
  methods = list(
    initialize = function(name, type, targets) {
      if (type == "%") {
        in_value <<- FALSE
      } else if (type == "&") {
        #Empty first for & nodes as we need to check its inputs
        in_value <<- logical(0)
      }
      state <<- FALSE
      name <<- name
      type <<- type
      targets <<- targets
    },
    show = function() {
      cat(paste("Node", name, "of type", type), "\n")
      cat(paste("Targeting", paste0(targets, collapse = ", ")), "\n")
      cat("The current input values are: ")
      cat(paste0(switch(type, "&" = names(in_value), "%" = "self"),
                 "=", in_value), "\n")
      cat("The current state is ", state)
    },
    pulse = function(signals) {
      if (!inherits(signals, "logical")) {
        rlang::abort("Signals must be logical value")
      }
      if (type == "%" && signals == FALSE) {
        state <<- !state
      } else if (type == "&") {
        #First update the in values
        in_value[names(signals)] <<- signals
        state <<- !all(in_value)
      }
    }
  )
)

