setwd(getwd())
library(ggplot2)


data <- read.table("productivity.txt", header = TRUE)

ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  labs(title = "Number of Species vs Productivity", x = "Productivity", y = "Number of Species")

correlation <- cor.test(data$x, data$y, method = "pearson")
print(correlation)

ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~f) +
  labs(title = "Number of Species vs Productivity by Region", x = "Productivity", y = "Number of Species")

