input <- readLines("day_24/00_input/input.txt")
test <- readLines("day_24/00_input/test.txt")

parse_input <- function(input_text) {
  tmp <- strsplit(input_text, " @ |,")
  out <- lapply(tmp, \(line) {
    matrix(
      as.numeric(line),
      nrow = 1, ncol = 6,
      dimnames = list(NULL, c("x", "y", "z", "vx", "vy", "vz"))
    )
  })
  do.call(rbind, out)
}

parsed_test <- parse_input(test)
parsed_input <- parse_input(input)
#Function to check how many intersection there might be
#Based on this:
#https://stackoverflow.com/questions/60840252/how-to-know-if-two-lines-intersect-using-r-is-there-an-equation-to-check-easily
intersect_line <- function(all_lines, min = 7, max = 27) {
  #First get the pairs of all lines so we can test
  pairs <- combn(nrow(all_lines), 2, simplify = FALSE)
  #Looping through all the pairs
  out <- lapply(pairs, \(pair) {
    p1 <- all_lines[pair[[1]], ]
    p2 <- all_lines[pair[[2]], ]
    #Calculating the slopes here
    s1 <- (p1[["vy"]] / p1[["vx"]])
    s2 <- (p2[["vy"]] / p2[["vx"]])
    #We know if slopes are parallel they will never intersect
    #Also need to lower the tolerance as the actual input might overflow
    if ( isTRUE(all.equal(s1, s2, tolerance = 1e-8))) {
      return(NA)
    }
    #Otherwise carry on calculating the number of intersections
    #The bias/constant
    b1 <- p1[["y"]] - s1*p1[["x"]]
    b2 <- p2[["y"]] - s2*p2[["x"]]
    intersect_x <- (b2-b1)/(s1-s2)
    intersect_y <- s1 * intersect_x + b1
    #Forward if this 2 lines will ever intersect
    fw1 <- ((intersect_x - p1[["x"]]) / p1[["vx"]]) > 0
    fw2 <- ((intersect_x - p2[["x"]]) / p2[["vx"]]) > 0
    if (fw1 && fw2) {
      return(c(intersect_x, intersect_y))
    } else {
      return(NA)
    }
  })
  Filter(\(c) all(dplyr::between(c, min, max)),
         x =  Filter(\(x) length(x) == 2, out)) |>
    length()
}

intersect_line(parsed_test, 7, 27) == 2
#Answer Part 1: 14799
intersect_line(parsed_input, 200000000000000, 400000000000000)
