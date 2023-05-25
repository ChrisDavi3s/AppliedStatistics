# Set seed for reproducibility
set.seed(123)

# Generate a large sample
sample_size <- 500
mu <- 0  # true mean
sigma <- 1  # true standard deviation
sample <- rnorm(sample_size, mean = mu, sd = sigma)

# Estimate the sample mean and standard deviation
sample_mean <- mean(sample)
sample_sd <- sd(sample)

# Compute the 95% confidence interval using z-score
z <- qnorm(0.975)  # z-score for 97.5th percentile of the normal distribution
error_margin_z <- z * sample_sd / sqrt(sample_size)
lower_bound_z <- sample_mean - error_margin_z
upper_bound_z <- sample_mean + error_margin_z

# Compute the 95% confidence interval using t-test command
t_test <- t.test(sample, conf.level = 0.95)

# Print the confidence intervals
cat("95% CI using z-score: [", lower_bound_z, ",", upper_bound_z, "]\n")
cat("95% CI using t.test: ", t_test$conf.int, "\n")



# Function to compare average CIs from normal and t methods
compare_cis_avg <- function(mu, sigma, sample_size, num_iterations=100, seed=123) {
  # Initialize variables to store the sum of lower and upper bounds
  sum_lower_z <- sum_upper_z <- sum_lower_t <- sum_upper_t <- 0
  
  # Set seed for reproducibility
  set.seed(seed)
  
  for (i in 1:num_iterations) {
    # Generate a sample
    sample <- rnorm(sample_size, mean = mu, sd = sigma)
    
    # Estimate the sample mean and standard deviation
    sample_mean <- mean(sample)
    sample_sd <- sd(sample)
    
    # Compute the 95% CI using z-score
    z <- qnorm(0.975)  # z-score for 97.5th percentile of the normal distribution
    error_margin_z <- z * sample_sd / sqrt(sample_size)
    lower_bound_z <- sample_mean - error_margin_z
    upper_bound_z <- sample_mean + error_margin_z
    
    # Compute the 95% CI using t-test command
    t_test <- t.test(sample, conf.level = 0.95)
    
    # Update the sum of lower and upper bounds
    sum_lower_z <- sum_lower_z + lower_bound_z
    sum_upper_z <- sum_upper_z + upper_bound_z
    sum_lower_t <- sum_lower_t + t_test$conf.int[1]
    sum_upper_t <- sum_upper_t + t_test$conf.int[2]
  }
  
  # Calculate the average confidence intervals
  avg_ci_z <- c(sum_lower_z / num_iterations, sum_upper_z / num_iterations)
  avg_ci_t <- c(sum_lower_t / num_iterations, sum_upper_t / num_iterations)
  
  # Print the average confidence intervals
  cat("Sample size: ", sample_size, ", mu: ", mu, ", sigma: ", sigma, ", iterations: ", num_iterations, "\n")
  cat("Average 95% CI using z-score: ", avg_ci_z, "\n")
  cat("Average 95% CI using t.test: ", avg_ci_t, "\n\n")
}

# Compare average CIs for different scenarios
compare_cis_avg(mu=0, sigma=1, sample_size=10000)
compare_cis_avg(mu=5, sigma=1, sample_size=10000)
compare_cis_avg(mu=0, sigma=4, sample_size=10000)

