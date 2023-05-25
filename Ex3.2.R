# Mean death count
mean_death_count <- mean(prussian$y)

# Standard deviation and sample size
std_dev <- sd(prussian$y)
sample_size <- length(prussian$y)

# 95% Confidence interval
error_margin <- qnorm(0.975) * std_dev / sqrt(sample_size)
lower_bound <- mean_death_count - error_margin
upper_bound <- mean_death_count + error_margin

# Output the mean and confidence interval
mean_death_count
lower_bound
upper_bound


