library(ggplot2)
library(dplyr)

sleep_ <- sleep
ggplot(sleep_, aes(x = ID, y = extra, color = group)) +
  geom_col()