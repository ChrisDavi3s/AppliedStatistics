# Load the required packages
library(ggplot2)
library(qqplotr)

# Q-Q plot of Michelson's Speed-of-Light Measurements using ggplot
ggplot(morley, aes(sample = Speed)) +
  stat_qq_line(color = "red") +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

