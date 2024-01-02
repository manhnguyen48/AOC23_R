### **Day 13**: Point of Incidence

-   Part 1: Today was another easy day as I parsed each grid as a matrix of characters. I then go through each line and split the matrix in 2 parts and flip the top parts. Either the top or bottom part has to be trimmed for comparison to check if we've found a mirror. This is done by taking the minimum of the number of rows of 2 parts. For columns we just have to transpose `t()` the matrix.

-   Part 2: Was relatively straightforward as I had to change the comparison condition to check if there is exactly one difference between the (trimmed) top and bottom part of the sliced matrix.
