### **Day 18**: Lavaduct Lagoon

-   Part 1 was quite straightforward as we just need to follow the instructions to find coordinates of all the points along the trench. Once we have that we can apply the Shoelace formula with Pick's theorem adjustment to find the area inside the trench. This also helped me complete Part 2 from Day 10.

-   Part 2 took a bit of thinking but once I realised Shoelace formula only requires coordinates of the points defining the polygons and we just need to keep track of the edges length it's relatively straightforward. I can see this would be a trap for anyone doing a flood fill algorithm implementation though.
