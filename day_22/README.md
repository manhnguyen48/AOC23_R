### Day 22: Sand Slabs

-   Part 1: This was another hard day for me as we had to first figure out how the blocks would drop before being able to figure out which one is safe to remove. This is the matter of looping through the bricks and look below if to see if it can fall down any further. Once that's done we need to check which brick is supported by which. If there are more than 2 bricks supporting a brick, we can remove until there is only 1 brick supporting the brick.

-   Part 2: If you've built a dependency graph in Part 1 then Part 2 should be easier as we basically do construct a chain reaction where removing one brick would drop the bricks above one by one according to the dependency graph.
