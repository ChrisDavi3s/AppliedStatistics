# Load dataset library
library(pscl)

# Compute the mean of the death count, excluding missing values
mean_death_count <- mean(prussian$y, na.rm = TRUE)

# Compute the standard deviation of the death count, excluding missing values
death_count_std_dev <- sd(prussian$y, na.rm = TRUE)

# Compute the sample size, excluding missing values
sample_size <- sum(!is.na(prussian$y))

# Compute the error margin for a 95% confidence interval
error_margin <- qnorm(0.975) * death_count_std_dev / sqrt(sample_size)

# Compute the lower and upper bounds of the 95% confidence interval
confidence_interval_lower_bound <- mean_death_count - error_margin
confidence_interval_upper_bound <- mean_death_count + error_margin

# Print the mean and confidence interval
print(paste0("Mean death count: ", mean_death_count))
print(paste0("95% Confidence Interval: [", confidence_interval_lower_bound, ", ", confidence_interval_upper_bound, "]"))


