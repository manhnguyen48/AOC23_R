### **Day 19**: Aplenty

-   Part 1 took some effort for in regex to parse the workflow into the format I need. Then it's the case of using a recursive function to check the condition of the parts. I used `eval` to evaluate the conditions here.

-   Part 2 was quite difficult as we could've faced combinatorial explosions with how many parts we need to check. But similar to Part 2 Day 5 we can solve this using just the ends of ranges. Each workflow basically split the parts into multiple ranges.
