source("day_10/01_script/shared_function.R")
input <- readLines("day_10/00_input/input.txt")
test <- readLines("day_10/00_input/test.txt")


parsed_test <- parse_input(test)
parsed_input <- parse_input(input)


#Figured out adjacent node by looking at where the S is
(length(dfs_traverse(parsed_test,
                     as.numeric(which(parsed_test == "S", TRUE)))))/2 == 8
#Answer Part 1: 6697
(length(dfs_traverse(parsed_input,
                     as.numeric(which(parsed_input == "S", TRUE)))))/2


