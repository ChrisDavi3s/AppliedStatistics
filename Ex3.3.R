# Define a function to compare average confidence intervals from normal and t methods over many iterations
compare_cis_avg <- function(population_mean, population_sd, sample_size, num_iterations=500, confidence_level=0.95, seed=123) {
  # Initialize variables to store the sum of lower and upper bounds for both methods
  sum_lower_z <- sum_upper_z <- sum_lower_t <- sum_upper_t <- 0
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Calculate the z-score for the specified percentile of the normal distribution
  z_score <- qnorm((1 + confidence_level) / 2)
  
  for (i in 1:num_iterations) {
    # Generate a sample
    sample_data <- rnorm(sample_size, mean = population_mean, sd = population_sd)
    
    # Calculate the sample mean and standard deviation
    sample_mean <- mean(sample_data)
    sample_sd <- sd(sample_data)
    
    # Compute the CI using z-score
    error_margin_z <- z_score * sample_sd / sqrt(sample_size)
    lower_bound_z <- sample_mean - error_margin_z
    upper_bound_z <- sample_mean + error_margin_z
    
    # Compute the CI using t-test command
    t_test_result <- t.test(sample_data, conf.level = confidence_level)
    
    # Add the lower and upper bounds to the sums
    sum_lower_z <- sum_lower_z + lower_bound_z
    sum_upper_z <- sum_upper_z + upper_bound_z
    sum_lower_t <- sum_lower_t + t_test_result$conf.int[1]
    sum_upper_t <- sum_upper_t + t_test_result$conf.int[2]
  }
  
  # Compute the average confidence intervals for both methods
  avg_ci_z <- c(sum_lower_z / num_iterations, sum_upper_z / num_iterations)
  avg_ci_t <- c(sum_lower_t / num_iterations, sum_upper_t / num_iterations)
  
  # Calculate the difference between the z and t methods
  ci_difference <- abs(avg_ci_z - avg_ci_t)
  
  # Print the average confidence intervals
  cat("Sample size: ", sample_size, ", mu: ", population_mean, ", sigma: ", population_sd, ", iterations: ", num_iterations, "\n")
  cat("Average", confidence_level * 100, "% CI using z-score: ", avg_ci_z, "\n")
  cat("Average", confidence_level * 100, "% CI using t.test: ", avg_ci_t, "\n")
  cat("Difference between z-score and t-test CIs: ", ci_difference, "\n\n")
}

# Use the function to compare average confidence intervals for different scenarios
compare_cis_avg(population_mean=0, population_sd=1, sample_size=100000)
compare_cis_avg(population_mean=5, population_sd=1, sample_size=100000)
compare_cis_avg(population_mean=0, population_sd=4, sample_size=100000)

# Change the confidence level to 0.60 to create a 60% confidence interval
compare_cis_avg(population_mean=0, population_sd=1, sample_size=100000, confidence_level=0.60)
