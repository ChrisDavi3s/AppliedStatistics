# Load required library
library(ggplot2)
library(dplyr)

# Set working directory
setwd(getwd())

# Load data from 'productivity.txt'
data <- read.table("productivity.txt", header = TRUE)

# Plot scatterplot of 'Number of Species' vs 'Productivity'
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  labs(x = "Productivity", y = "Number of Species")+
  theme_minimal()

# Perform and print Pearson correlation test
correlation <- cor.test(data$x, data$y, method = "pearson")
print(correlation)

# Plot scatterplot of 'Number of Species' vs 'Productivity' for each group in 'f'
ggplot(data, aes(x = x, y = y)) +
  geom_point() +
  facet_wrap(~f) +
  labs(x = "Productivity", y = "Number of Species")+
  theme_minimal()

# Run a correlation test for each group
grouped_data <- split(data, data$f)
results <- data.frame(f = names(grouped_data))
results$correlation <- lapply(grouped_data, function(df) cor.test(df$x, df$y))

# Extract correlation estimate and p-value from each test
results$cor <- sapply(results$correlation, function(x) x$estimate)
results$p_value <- sapply(results$correlation, function(x) x$p.value)

# Print the results
print(results)


