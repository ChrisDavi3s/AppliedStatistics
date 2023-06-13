# Required Libraries
library(ggplot2)
library(RColorBrewer)
library(viridis)

# Draw a sample of 5 numbers from a normal distribution with standard deviation 1 and mean 0.5.
sample <- rnorm(n = 5, mean = 0.5, sd = 1)

# Run a t-test on your sample at the 5% significance level, 
# taking the null hypothesis to be that the population mean is zero.
t_test <- t.test(sample, mu = 0)

# print the t-test results
print(t_test)

# Set the seed for reproducibility
set.seed(123)  


# Function to run t-test and calculate power
calculate_power <- function(mu, sigma, sample_size, num_iterations=500) {
  run_t_test <- function() {
    # Draw a sample
    sample <- rnorm(sample_size, mean = mu, sd = sigma)
    
    # Run a t-test and return the p-value
    t_test <- t.test(sample, mu = 0, alternative = "two.sided", conf.level = 0.95)
    return(t_test$p.value)
  }
  
  # Replicate the t-test many times
  p_values <- replicate(num_iterations, run_t_test())
  
  # Count the number of significant results (true positives)
  num_true_positives <- sum(p_values < 0.05)
  
  # Calculate the power
  power <- num_true_positives / num_iterations
  return(power)
}

# Values for the first part of the Exercise
calculate_power(mu = 0.5, sigma = 1, sample_size = 5, num_iterations = 10000)

# Set parameter ranges
sigma_values <- seq(0.1, 2, by = 0.1)
mu_values <- seq(-2, 2, by = 0.1)
sample_sizes <- seq(5, 50, by = 5)

# Calculate power for varying parameters
power_vs_sigma <- sapply(sigma_values, function(sigma) calculate_power(mu = 0.5, sigma = sigma, sample_size = 30))
power_vs_mu <- sapply(mu_values, function(mu) calculate_power(mu = mu, sigma = 1, sample_size = 30))
power_vs_sample_size <- sapply(sample_sizes, function(sample_size) calculate_power(mu = 0.5, sigma = 1, sample_size = sample_size))

# Create ggplot2 plots
df_sigma <- data.frame(sigma = sigma_values, power = power_vs_sigma)
ggplot(df_sigma, aes(x=sigma, y=power)) + 
  geom_line() +
  labs( x = "Sigma", y = "Power") +
  theme_minimal()

df_mu <- data.frame(mu = mu_values, power = power_vs_mu)
ggplot(df_mu, aes(x=mu, y=power)) + 
  geom_line() +
  labs( x = "Mu", y = "Power") +
  theme_minimal()

df_sample_size <- data.frame(sample_size = sample_sizes, power = power_vs_sample_size)
ggplot(df_sample_size, aes(x=sample_size, y=power)) + 
  geom_line() +
  labs( x = "Sample Size", y = "Power") +
  theme_minimal()

# Define the grid of parameters
sigma_values <- seq(0.1, 3, by = 0.1)
mu_values <- seq(-2, 2, by = 0.1)

# Initialize a matrix to hold the power values
power_matrix <- matrix(nrow = length(sigma_values), ncol = length(mu_values))

# Compute the power for each combination of sigma and mu
for (i in 1:length(sigma_values)) {
  for (j in 1:length(mu_values)) {
    power_matrix[i, j] <- calculate_power(mu = mu_values[j], sigma = sigma_values[i], sample_size = 20)
  }
}

# Transpose the matrix so that rows correspond to the x-axis and columns correspond to the y-axis
power_matrix <- t(power_matrix)

# Create the 2D plot (heat map)
image(x = mu_values, y = sigma_values, z = power_matrix, 
      xlab = "Mu", ylab = "Sigma",
      col = viridis(100))

# Add contour lines to the plot to make it easier to read
contour(x = mu_values, y = sigma_values, z = power_matrix, add = TRUE)

# Define the grid of parameters
sample_sizes <- seq(5, 150, by = 5)
mu_values <- seq(-2, 2, by = 0.1)

# Initialize a matrix to hold the power values
power_matrix <- matrix(nrow = length(sample_sizes), ncol = length(mu_values))

# Compute the power for each combination of sigma and mu
for (i in 1:length(sigma_values)) {
  for (j in 1:length(mu_values)) {
    power_matrix[i, j] <- calculate_power(mu = mu_values[j], sigma = 1, sample_size = sample_sizes[i])
  }
}

# Transpose the matrix so that rows correspond to the x-axis and columns correspond to the y-axis
power_matrix <- t(power_matrix)

# Create the 2D plot (heat map)
image(x = mu_values, y = sample_sizes, z = power_matrix, 
      xlab = "Mu", ylab = "Sample Size",
       col = viridis(100))

# Add contour lines to the plot to make it easier to read
contour(x = mu_values, y = sigma_values, z = power_matrix, add = TRUE)


