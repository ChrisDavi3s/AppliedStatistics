# Load the necessary libraries
library(ggplot2)
library(qqplotr)

# Set seed for reproducibility
set.seed(42)

# Define the parameters for the normal distribution from which we will draw samples
small_sample_size <- 3
num_replications <- 100000
population_mean <- 0
population_sd <- 2

# Generate a large number of samples and compute the standard deviation of each
small_sample_sds <- replicate(num_replications, {
  sample_data <- rnorm(small_sample_size, mean = population_mean, sd = population_sd)
  sd(sample_data)
})

# Put the computed standard deviations into a data frame
small_sample_sds_df <- data.frame(small_sample_sds)

# Plot a histogram of the small sample standard deviations
ggplot(small_sample_sds_df, aes(x=small_sample_sds)) + 
  geom_histogram(bins=50, fill = "gray", color = "black") +
  labs( x = "Sample Standard Deviation", y = "Frequency") +
  theme_minimal()

# Generate a QQ plot of the small sample standard deviations
ggplot(small_sample_sds_df, aes(sample = small_sample_sds)) +
  stat_qq_line(color = "red") +
  stat_qq_point() +
  labs( x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Compute and print the mean of the sample standard deviations
mean_small_sample_sds <- mean(small_sample_sds)
mean_small_sample_sds

# Repeat the above process, but with a larger sample size
set.seed(42)
large_sample_size <- 30

large_sample_sds <- replicate(num_replications, {
  sample_data <- rnorm(large_sample_size, mean = population_mean, sd = population_sd)
  sd(sample_data)
})

# Put the computed standard deviations into a new data frame
large_sample_sds_df <- data.frame(large_sample_sds)

# Plot a histogram of the larger sample's standard deviations
ggplot(large_sample_sds_df, aes(x=large_sample_sds)) + 
  geom_histogram(bins=50, fill = "gray", color = "black") +
  labs(x = "Sample Standard Deviation", y = "Frequency") +
  theme_minimal()

# Generate a QQ plot of the large sample standard deviations",
ggplot(large_sample_sds_df, aes(sample = large_sample_sds)) +
  stat_qq_line(color = "red") +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
