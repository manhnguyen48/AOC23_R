### **Day 08**: Haunted Wasteland

-   Part 1 I thought I finally have to use some graph theory but turns out traversing the network was just a case of accessing a named list.

-   Part 2 was a bit more tricky as I quickly find out there are roughly 20 billion steps I would need to traverse to calculate this brute force. However, after some tinkling, I figured it takes the same number of steps to find the first node ending with Z (`.*Z$`) as it takes to get from the last node to the first node. Which means our answer is the least common multiple (LCM) of the steps for all 6 ghosts. I used `pracma::Lcm` for this implementation.
