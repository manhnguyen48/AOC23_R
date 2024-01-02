### **Day 16**: The Floor Will Be Lava

-   Part 1 was relative straightforward breadth first search of the 2D array. But we also need to store the position `(i,j)` in the matrix along with the direction of travel `c(0,1) c(1,0) c(-1,0) c(0,-1)` as we might hit the same tile in different directions. I was worried in Part 2 they would ask us to reposition the mirrors or the light bounce back from the edge.

-   Part 2 was brute force again. I've tried `furrr::furrr_map_dbl` for parallelism but that doesn't seem to improve the speed. Parallel processing in R has a long way to go compared to other languages. You probably could skipped some starting points early if the mirrors are reflecting in a circle somehow.
