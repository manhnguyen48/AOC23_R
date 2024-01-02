### **Day 03**: Gear Ratios

-   Part 1 was a bit of a struggle as I haven't had to do a 2D array traversing in my job. But I do have experience with regex. The core idea is to find a complete numbers (2-3 digits next to each other) and look around if it's surrounded by any special characters.

-   Part 2 was a bit of reverse of Part 1. We first find locations of the stars (`*`) and search around for 2 numbers. We can ignore stars that are only surrounded by 1 number. Because the area of search might not encompass the full numbers, we return the absolute position of those numbers. We then use `grep` to search for numbers in the 3 lines around our stars and find our complete numbers then.
