### **Day 01**: Trebuchet

-   Part 1 was pretty straightforward so I tried to benchmark performance between base R and `stringi` solutions. Turns out `stringi` is just as fast as base R.

-   Part 2 was less obvious, I had to create an incidence matrix of where the numeric pattern was first found and last found. This is to circumvent cases where "sevenine" -\> 79
