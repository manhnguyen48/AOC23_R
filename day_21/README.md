### **Day 21**: Step Counter

-   Part 1 was a standard breadth first search and keep track of how many steps we've travelled.

-   Part 2 was again quite challenging as the steps now increase a lot and the input grid also repeat infinitely in all directions. This required some inspection of the inputs to figure out there is a clear line horizontal and vertical from the starting point, we start from the middle of the grid and there is an empty diamond shape around the centre which takes 65 steps to reach. This means the number of plots reachable will be a quadratic formula: y = ax\^2 + bx where x is the number of grids we would reach and y is the number of plots reachable. We only need to manually search the first 3 values 0-2 and plug the results in `lm` . The `bfs` function from Part 1 will need some modifications to search for the plots in arbitrary number of grids.
