# Load the required packages
library(ggplot2)
library(qqplotr)

# Create a Q-Q plot using ggplot2
ggplot(morley, aes(sample = Speed)) +
  stat_qq_line(color = "red") +
  stat_qq_point() +
  labs(title = "Q-Q plot of Michelson's Speed-of-Light Measurements",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

