input <- readr::read_file("day_19/00_input/input.txt")
test <- readr::read_file("day_19/00_input/test.txt")
#Similar to part 1 but we don't need the part specs any more
parse_input <- function(input_text) {
  tmp <- strsplit(input_text, "(\r)?\n(\r)?\n")[[1]]
  wf <- hash::hash()
  for (f in strsplit(tmp[1], "(\r)?\n")[[1]]) {
    node_name <- stringi::stri_extract(f, regex = "[a-z]+(?=\\{)")
    node_rule <- list()
    tmp2 <- stringi::stri_extract(f, regex = "(?<=\\{).*(?=\\})") |>
      strsplit(",") |> unlist()
    node_rule[["ruleset"]] <- tmp2[1:(length(tmp2) - 1)] |>
      unlist() |>
      #Splitting by both the operator > and :
      strsplit("(\\>|\\<|\\:)")
    node_rule[["fallback"]] <- tmp2[[length(tmp2)]]
    wf[[node_name]] <- node_rule
  }
  return(wf)
}
parsed_test <- parse_input(test)
parsed_input <- parse_input(input)

# 256 trillion combo we can't test manually
part_spec_range <- list("x" = c(1,4000),
                        "m" = c(1,4000),
                        "a" = c(1,4000),
                        "s" = c(1,4000))

prod_range <- function(spec_range) {
  vapply(spec_range, \(rng) rng[[2]] - rng[[1]] + 1,numeric(1)) |>
    prod()
}
#Modified the recursion to work with a range of values instead
follow_rule_range <- function(step_name, part_spec_range, wf) {
  #Stop condition
  if (step_name == "A")
    return(prod_range(part_spec_range))
  if (step_name == "R")
    return(0)
  ruleset <- wf[[step_name]][["ruleset"]]
  fallback <- wf[[step_name]][["fallback"]]
  total <- 0
  #Recursively slicing the range
  for (rule in ruleset) {
    part_attr <- rule[[1]]
    cmp_op <- stringi::stri_extract(rule[[2]], regex = "<|>")
    value <-
      stringi::stri_extract(rule[[2]], regex = "\\d+") |> as.numeric()
    target <- rule[[3]]
    passed_range <- part_spec_range
    failed_range <- part_spec_range
    if (cmp_op == ">") {
      #If greater than then we modify the lower of the range
      passed_range[[part_attr]][[1]] <- value + 1
      failed_range[[part_attr]][[2]] <- value
    } else if (cmp_op == "<") {
      #Else we modify the upper of the range
      passed_range[[part_attr]][[2]] <- value - 1
      failed_range[[part_attr]][[1]] <- value
    }
    total <- total + follow_rule_range(target, passed_range, wf)
    #Pass along the failed range to the next rule/loop
    part_spec_range <- failed_range
  }
  #If we're here means we've hit the fall back
  total <- total + follow_rule_range(fallback, part_spec_range, wf)
  return(total)
}

follow_rule_range("in", part_spec_range, parsed_test) == 167409079868000
#Answer Part 2: 132,186,256,794,011
follow_rule_range("in", part_spec_range, parsed_input)
