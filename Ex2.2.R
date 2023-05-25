set.seed(42) # Optional: Set a random seed for reproducibility
sample_size <- 3
num_samples <- 100000
true_mean <- 0
true_sd <- 2

sampleSDs <- replicate(num_samples, {
  sample_data <- rnorm(sample_size, mean = true_mean, sd = true_sd)
  sd(sample_data)
})

hist(sampleSDs, main = "Histogram of Sample Standard Deviations", xlab = "Sample Standard Deviation", breaks = 50)

qqnorm(sampleSDs, main = "Q-Q plot of Sample Standard Deviations", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(sampleSDs, col = "red")

mean_sampleSDs <- mean(sampleSDs)
mean_sampleSDs

set.seed(42) # Optional: Set a random seed for reproducibility
sample_size_large <- 30

sampleSDs_large <- replicate(num_samples, {
  sample_data <- rnorm(sample_size_large, mean = true_mean, sd = true_sd)
  sd(sample_data)
})

hist(sampleSDs_large, main = "Histogram of Sample Standard Deviations (n = 30)", xlab = "Sample Standard Deviation", breaks = 50)

