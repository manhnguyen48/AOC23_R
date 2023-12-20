input <- readr::read_file("day_19/00_input/input.txt")
test <- readr::read_file("day_19/00_input/test.txt")
#Some regex needed to parse the inputs here
#workflow is stored as a hash for quicker access later
#For ruleset, the last item is fallback, each rule is parsed
#as c(attr_name, comparison, target_if_tru)
parse_input <- function(input_text) {
  tmp <- strsplit(input_text, "(\r)?\n(\r)?\n")[[1]]
  wf <- hash::hash()
  for (f in strsplit(tmp[1], "(\r)?\n")[[1]]) {
    node_name <- stringi::stri_extract(f, regex = "[a-z]+(?=\\{)")
    node_rule <- list()
    tmp2 <- stringi::stri_extract(f, regex = "(?<=\\{).*(?=\\})") |>
      strsplit(",") |> unlist()
    node_rule[["fallback"]] <- tmp2[[length(tmp2)]]
    node_rule[["ruleset"]] <- tmp2[1:(length(tmp2) - 1)] |>
      unlist() |>
      #Splitting by both the operator > and :
      strsplit("(\\>|\\<|\\:)")
    wf[[node_name]] <- node_rule
  }
  #Parsing the parts as matrix of attributes
  parts <- gsub("\\{|\\}", "", strsplit(tmp[2],  "(\r)?\n")[[1]]) |>
    strsplit(",") |>
    lapply(\(x) stringi::stri_extract(x, regex = "\\d+") |>
             as.numeric()) |>
    unlist() |> matrix(ncol = 4,
                       byrow = TRUE,
                       dimnames = list(NULL, c("x", "m", "a", "s")))
  #For parts remove curly braces and add to matrix
  return(list("wf" = wf, "parts" = parts))
}

parsed_test <- parse_input(test)
parsed_input <- parse_input(input)

#Recursive function to check the rules
follow_rule <- function(step_name, part_spec, wf) {
  #Stop condition if we arrive at classification
  if (step_name == "A") return(TRUE)
  if (step_name == "R") return(FALSE)
  ruleset <- wf[[step_name]][["ruleset"]]
  fallback <- wf[[step_name]][["fallback"]]
  #Looping through rules to find the next target to go to
  for (rule in ruleset) {
    part_attr <- rule[[1]]
    exprs <- rule[[2]]
    target <- rule[[3]]
    #Don't eval raw texts in production
    if (eval(parse(text = paste0(part_spec[[part_attr]], exprs)))) {
      return(follow_rule(target, part_spec, wf))
    }
  }
  #If we're here means we haven't hit any of the rules, then use the fall back
  return(follow_rule(fallback, part_spec, wf))
}
#Function to run classification on all parts and calculate sum of attributes
sort_parts <- function(input_list) {
  wf <- input_list[["wf"]]
  parts <- input_list[["parts"]]
  #This will return boolean vectors indicating if part passes the checks
  classification <- apply(parts, 1, \(x) follow_rule("in", x, wf))
  parts[classification,] |> sum()
}

sort_parts(parsed_test) == 19114
#Answer Part 1: 495298
sort_parts(parsed_input)

