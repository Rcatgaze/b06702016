library(ggplot2)
library(dplyr)

women_ <- women
ggplot(women, aes(x = height, y = weight)) +
  geom_line()
