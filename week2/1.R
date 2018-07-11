library(ggplot2)
library(dplyr)

trees_height_Girth <- trees 
ggplot(trees_height_Girth, aes(x = Girth, y = Height, size = Volume)) +
  geom_point()