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

library(caracas)
solve_system <- function(hailstones) {
  #We only need 6 equations to solve for 6 variables, and 3 stones is enough to
  #form 6 equations
  stones <- hailstones[1:3, ]

  # Define symbols for the position and velocity of rock
  xr <- caracas::symbol("xr")
  yr <- caracas::symbol("yr")
  zr <- caracas::symbol("zr")
  vxr <- caracas::symbol("vxr")
  vyr <- caracas::symbol("vyr")
  vzr <- caracas::symbol("vzr")

  # Construct the lhs and rhs of  system of equations row by row
  lhs <- list()
  rhs <- list()
  for (i in 1:nrow(stones)) {
    lhs[[2*i-1]] <- (xr - stones[i, "x"]) * (stones[i, "vy"] - vyr)
    lhs[[2*i]] <- (yr - stones[i, "y"]) * (stones[i, "vz"] - vzr)
    rhs[[2*i-1]] <- (yr - stones[i, "y"]) * (stones[i, "vx"] - vxr)
    rhs[[2*i]] <- (zr - stones[i, "z"]) * (stones[i, "vy"] - vyr)
  }
  # Use the solve_sys function from caracas to solve the system of equations
  solutions <- caracas::solve_sys(do.call(cbind, lhs),
                                  do.call(cbind, rhs),
                                  list(xr, yr, zr, vxr, vyr, vzr))
  #Check for integer solutions, there might be a better way to add this constraints
  #in the solver instead
  for (s in solutions) {
    xrs <- as.character(s$xr)
    yrs <- as.character(s$yr)
    zrs <- as.character(s$zr)
    vxrs <- as.character(s$vxr)
    vyrs <- as.character(s$vyr)
    vzrs <- as.character(s$vzr)
    #If we find a divisor, the solution is not integer so skip
    if (any(grepl("\\/", c(xrs, yrs, zrs, vxrs, vyrs, vzrs)) )) {
      next
    }
    out <- sum(as.numeric(c(xrs,yrs,zrs)))
  }
  return(out)
}

solve_system(parsed_test) == 47
#Answer Part 2: 1007148211789625
solve_system(parsed_input)
