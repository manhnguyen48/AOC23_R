### Day 23: A Long Walk

-   Part 1 was quite interesting as I've always learned about shortest paths in graph theory. Longest path was eye-openning as I had to figure out how to make the solution tractable by contract the graph. Basically if we can find all the "crossroad" nodes, starting node, and ending nodes to construct our graph. Other points don't matter as there are only 1 way through them. Part 1 was a bit easier to brute force with the help of the arrows (`> < ^ v` ). There is probably a clever way to incorporate negative weights with `igraph` to find the longest path and I'd be back to measure efficiency of that approach.

-   Part 2 was pure brute force as we ignore the arrows in our path construction.
