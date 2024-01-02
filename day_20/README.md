### **Day 20**: Pulse Propagation

-   Part 1 was quite fun and probably the most unique puzzle I've seen from Day 1. I played around with the OOP in R using R6 system to construct a node object with some custom methods. This system however modify the object in place so I probably should have made copies of the object before returning it.

-   Part 2 was quite challenging as it wasn't obvious how to find the rhythm of the nodes. But upon seeing some visualisations in the subreddit, it becomes clear there are only 4 nodes indirectly influencing the rx node and we need to find when do they all send a low signal. LCM was on the menu again.
