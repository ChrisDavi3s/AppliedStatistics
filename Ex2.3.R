# Load necessary library
library(ggplot2)

# Define parameters
cauchy_sample_size <- 100
num_replications <- 1000

# Generate means of several samples from Cauchy distribution
cauchy_sample_means <- replicate(num_replications, {
  cauchy_sample <- rcauchy(cauchy_sample_size)
  mean(cauchy_sample)
})

# Create dataframe for ggplot
cauchy_means_df <- data.frame(cauchy_sample_means)

#Q-Q Plot of Sample Means (Cauchy Distribution) using ggplot
ggplot(cauchy_means_df, aes(sample = cauchy_sample_means)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  coord_cartesian(xlim = c(-5, 5), ylim=c(-300,300)) +
  theme_minimal()

