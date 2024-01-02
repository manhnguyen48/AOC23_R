### Day 24: Never Tell Me The Odds

-   Part 1: was relatively straightforward as we just need to find the coordinates at which 2 lines intersects. Treating each hailstone as a line, I checked the line intersection thanks to answers on stackoverflow for all pairs.

-   Part 2: was very interesting as I learned we had to solve a system of equations for 6 unknown variables representing the initial position of our rock. I was playing around with both `Ryacas` and `caracas` but it seems `caracas` was easier to use as it's closer to `sympy`. `Ryacas` was able to construct all the equations from matrix without needing to loop though. Also, we only need 6 equations to solve for 6 variables which can be obtained from the first 3 rocks. Our position has to be integer solution so I had to do some loop to check for them - I remember in `sympy` you can add constraint for integer solution but apparently not possible in `caracas`.
