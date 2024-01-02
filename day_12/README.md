### **Day 12**: Hot Springs

-   Part 1 I was stuck for a while as this problem was quite odd. But after stumbling on the base R function `rle` things become clear. I basically create all permutations of possible values ? could have taken and remove ones that don't satisfy the number of broken gears left. I knew I was walking into a trap but couldn't think of any better way of doing it then.

-   Part 2 Admittedly I had to take some hints on recursion and caching to get this part done. I was following hyper-neutrino [code](https://github.com/hyper-neutrino/advent-of-code/blob/main/2023/day12p2.py) but stuck on translating it to R due to different indices so I used Andrea Barghetti's [R version](https://github.com/AndreaBarghetti/AdventOfCode/blob/main/AoC2023/Day12/day12.R)
