### **Day 07**: Camel Cards

-   Part 1 was quite straightforward and I made use of `ordered factor` in R. We just need to assign each card a value 1-13 and string pad them so 2 -\> "02". Each hand of 5 cards is then concatenated so smaller cards will have smaller numbers. We then rank the hand based on the original card and this was just the case of assessing the top 2 values in a frequency tables.

-   Part 2 was a bit more tricky: first thing is to change the power level of J by moving it before 2 in the `ordered factor` . In the hand classification step, we need to account for the number of J, if we have any AND less than 5, J would become whatever the other most frequent card is.
