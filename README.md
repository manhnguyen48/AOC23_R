# Advent of Code 2023 Solutions in R

This is my first year solving [Advent of Code](https://adventofcode.com/2023) puzzles in R. I will try my best to complete both puzzles every day but my time is limited while working full time.

This is set up as an RStudio Project. The `utils.R` script contains the `set_up_day()` function to help download inputs and set up the folder each day. My session ID is stored as an environment variable in a `.Renviron` file that is not committed to this repo. The input files `input.txt` are not included as it's the author of AOC's IP.

Across all solutions, I try to make use of base R and tidyverse where relevant, add comments where it's not obvious and run my functions through test cases. I will try to revisit some days to rework solutions to be more optimal when time allows. Below are my reflections on each day:

### **Day 01**: Trebuchet

<details>

<summary>Details</summary>

-   Part 1 was pretty straightforward so I tried to benchmark performance between base R and `stringi` solutions. Turns out `stringi` is just as fast as base R.

-   Part 2 was less obvious, I had to create an incidence matrix of where the numeric pattern was first found and last found. This is to circumvent cases where "sevenine" -\> 79

</details>

### **Day 02**: Cube Conundrum

<details>

<summary>Details</summary>

-   Part 1 was quite easy, I just make a function to count the number of cubes in each set and add them to a data frame. Then a function to take the maximum number of cubes seen in each colours across all sets. This is then used to compare against a criteria

-   Part 2 can reuse most of part 1, but instead of filtering we just take the maximum number of cubes seen as the minimum number needed to make the game possible.

</details>

### **Day 03**: Gear Ratios

<details>

<summary>Details</summary>

-   Part 1 was a bit of a struggle as I haven't had to do a 2D array traversing in my job. But I do have experience with regex. The core idea is to find a complete numbers (2-3 digits next to each other) and look around if it's surrounded by any special characters.

-   Part 2 was a bit of reverse of Part 1. We first find locations of the stars (`*`) and search around for 2 numbers. We can ignore stars that are only surrounded by 1 number. Because the area of search might not encompass the full numbers, we return the absolute position of those numbers. We then use `grep` to search for numbers in the 3 lines around our stars and find our complete numbers then.

</details>

### **Day 04**: Scratchcards

<details>

<summary>Details</summary>

-   Part 1 was quite easy, we just need to count how many match between the cards we have and the winning cards.

-   Part 2 I thought I had to write a recursive functions but turns out updating an array of card counts in a loop works just as well.

</details>

### **Day 05**: If You Give A Seed A Fertilizer

<details>

<summary>Details</summary>

-   Part 1 I fell for the trap of writing a mapping function for a single values but it still work. Also I probably spent too much time writing the parsing function to read the maps as data.frame.

-   Part 2 I haven't completed as it was a full day in the office + some travel, but I have the general idea that Part 1 is just a special case of Part 2, and we only need to map the end values instead of the entire range.

</details>

### **Day 06**: Wait For It

<details>

<summary>Details</summary>

-   Part 1 this was another easy day as we just need to count how many possible values are above the record. I implemented a brute force algorithm in part 1.

-   Part 2 I thought I had to change my code as it would've been more efficient to formulate as a quadratic equation problem where we solve for the load time. Turns out my brute force algorithm is quick enough to complete part 2 in \<2s so I'm not too bothered.

</details>

### **Day 07**: Camel Cards

<details>

<summary>Details</summary>

-   Part 1 was quite straightforward and I made use of `ordered factor` in R. We just need to assign each card a value 1-13 and string pad them so 2 -\> "02". Each hand of 5 cards is then concatenated so smaller cards will have smaller numbers. We then rank the hand based on the original card and this was just the case of assessing the top 2 values in a frequency tables.

-   Part 2 was a bit more tricky: first thing is to change the power level of J by moving it before 2 in the `ordered factor` . In the hand classification step, we need to account for the number of J, if we have any AND less than 5, J would become whatever the other most frequent card is.

</details>

### **Day 08**: Haunted Wasteland

<details>

<summary>Details</summary>

-   Part 1 I thought I finally have to use some graph theory but turns out traversing the network was just a case of accessing a named list.

-   Part 2 was a bit more tricky as I quickly find out there are roughly 20 billion steps I would need to traverse to calculate this brute force. However, after some tinkling, I figured it takes the same number of steps to find the first node ending with Z (`.*Z$`) as it takes to get from the last node to the first node. Which means our answer is the least common multiple (LCM) of the steps for all 6 ghosts. I used `pracma::Lcm` for this implementation.

</details>

### **Day 09**: Mirage Maintenance

<details>

<summary>Details</summary>

-   Part 1 This was a surprisingly easy day, part 1 was a `Reduce("+",...)` over the ending diff values

-   Part 2 was just reversed so I changed to `Reduce(\(x,y) y-x, ...)` over the starting diff values

</details>

### **Day 10**: Pipe Maze

<details>

<summary>Details</summary>

-   Part 1 After yesterday, this level of difficulty was expected. I spent some time implementing a graph using `igraph` in Part 1, worrying that Part 2 would use some fancy maths that would require graph computation. It's been a while since I last touched graph theory so it was quite fun.

-   Part 2 I haven't had the chance to complete as was stuck debugging why the [ray cast algorithm](https://en.wikipedia.org/wiki/Point_in_polygon) was not working. Probably because of some edge cases with the symbols I need to account for. But we're almost there.

</details>

### **Day 11**: Cosmic Expansion

<details>

<summary>Details</summary>

-   Part 1 We have another easy day and I have not fallen for the trap. I knew this expansion factor would be ridiculous in part 2 somehow so I made a function to keep track of the indices after expansion then extract all the galaxies' coordinates (`#`). After all the problem is just on a Cartesian plane of coordinates. Base R has a `dist` function that can calculate [Manhattan distance](https://en.wikipedia.org/wiki/Taxicab_geometry) matrix.

-   Part 2 It is so satisfying to just reuse everything in Part 1 and just change one number.

</details>

### **Day 12**: Hot Springs

<details>

<summary>Details</summary>

-   Part 1 I was stuck for a while as this problem was quite odd. But after stumbling on the base R function `rle` things become clear. I basically create all permutations of possible values ? could have taken and remove ones that don't satisfy the number of broken gears left. I knew I was walking into a trap but couldn't think of any better way of doing it then.

-   Part 2 Admittedly I had to take some hints on recursion and caching to get this part done. I was following hyper-neutrino [code](https://github.com/hyper-neutrino/advent-of-code/blob/main/2023/day12p2.py) but stuck on translating it to R due to different indices so I used Andrea Barghetti's [R version](https://github.com/AndreaBarghetti/AdventOfCode/blob/main/AoC2023/Day12/day12.R)

</details>

### **Day 13**: Point of Incidence

<details>

<summary>Details</summary>

-   Part 1: Today was another easy day as I parsed each grid as a matrix of characters. I then go through each line and split the matrix in 2 parts and flip the top parts. Either the top or bottom part has to be trimmed for comparison to check if we've found a mirror. This is done by taking the minimum of the number of rows of 2 parts. For columns we just have to transpose `t()` the matrix.

-   Part 2: Was relatively straightforward as I had to change the comparison condition to check if there is exactly one difference between the (trimmed) top and bottom part of the sliced matrix.

</details>

### **Day 14**: Parabolic Reflector Dish

<details>

<summary>Details</summary>

-   Part 1 Was relatively straightforward, I imagined the tilting operation would be for each column, treat it as a single string, split it by the `"#"` character, the reorder the individual smaller parts. Then to calculate the load on the north side, we search the row indices of all the `O` then reverse code it based on the number of rows; and add everything together.

-   Part 2 was a lot of work, but I'm relieved I can reuse most of part 1, I was worrying the space between rocks would expand like they did with the universe problem on day 11 and make part 1 obsolete. First we need a function the would perform a cycle: North \> West \> South \> East means we're in loop of counter clockwise. Because of the rocks, we have a sets of bigger cycles where we would run into the same states i.e. the mirrors would "settle" into some position. Therefore, it's a case of keep cycling and recording the unique states we've run into. I capture a state by concatenate the whole matrix into a single string and also record the index of when we run into them.

</details>

### **Day 15**: Lens Library

<details>

<summary>Details</summary>

-   Part 1 was surprisingly easy and probably the shortest part 1 so far. We just need to write a short custom hash function using Reduce.

-   Part 2 was basically an introduction to hash tables and I took this opportunity to test out R `utils::hashtab` although it's still in experimental stage. It seems quite straightforward to me but you could've done part 2 with just a common list or vector. I suppose it's more useful when frequent data access and checks are needed. Reading/Understanding the problem was harder than the solution itself. It will be hard from now on though.

</details>

### **Day 16**: The Floor Will Be Lava

<details>

<summary>Details</summary>

-   Part 1 was relative straightforward breadth first search of the 2D array. But we also need to store the position `(i,j)` in the matrix along with the direction of travel `c(0,1) c(1,0) c(-1,0) c(0,-1)` as we might hit the same tile in different directions. I was worried in Part 2 they would ask us to reposition the mirrors or the light bounce back from the edge.

-   Part 2 was brute force again. I've tried `furrr::furrr_map_dbl` for parallelism but that doesn't seem to improve the speed. Parallel processing in R has a long way to go compared to other languages. You probably could skipped some starting points early if the mirrors are reflecting in a circle somehow.

</details>

### **Day 17**: Clumsy Crucible

<details>

<summary>Details</summary>

-   Part 1 was quite interesting. I thought it would be the case like day 10 again where we use graph but turns out calculating the edge list for `igraph` might take more time as these consecutive number of steps could change and I would repeat some edge pairs when looping through the matrix. So I learned about the details of [Dijkstra's](https://en.wikipedia.org/wiki/Dijkstra's_algorithm) algorithm and how to customise it. Seems quite similar to the breadth first search yesterday and but we need to use a priority queue instead and `collections::priority_queue` was brilliant. Also we can assign any priority value explicitly by adding `-` to the current total heat loss value because it's a max-heap queue.

-   Part 2 was relatively straightforward, we need to change a few parameters for the code: first the exit conditions is that we need to hit at least 4 consecutive steps; second is we can now take 10 steps at a time; and third is we need to hit minimum 4 consecutive steps before considering turning directions. Overall quite fun day!

</details>
