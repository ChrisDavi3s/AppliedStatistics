library(RColorBrewer)
library(viridis)


set.seed(123)  # for reproducibility

# Draw a sample
sample_size <- 5
mu <- 0.5
sigma <- 1
sample <- rnorm(sample_size, mean = mu, sd = sigma)

# Run a t-test
t_test <- t.test(sample, mu = 0, alternative = "two.sided", conf.level = 0.95)

# Print the t-test result
print(t_test)


#The t-test will output a p-value. If this p-value is less than 0.05 (indicating that the test is significant at the 5% level), then we reject the null hypothesis that the population mean is zero. In reality, we know that the population mean is 0.5 (since we drew the sample from a normal distribution with mean 0.5), so if we reject the null hypothesis, then we have a true positive. If we fail to reject the null hypothesis, then we have a false negative (also known as a Type II error).


set.seed(123)  # for reproducibility

# Initialize count of true positives
num_true_positives <- 0
num_iterations <- 1000

for (i in 1:num_iterations) {
  # Draw a sample
  sample <- rnorm(sample_size, mean = mu, sd = sigma)
  
  # Run a t-test
  t_test <- t.test(sample, mu = 0, alternative = "two.sided", conf.level = 0.95)
  
  # Check if it's a true positive
  if (t_test$p.value < 0.05) {
    num_true_positives <- num_true_positives + 1
  }
}

# Calculate the power
power <- num_true_positives / num_iterations

# Print the power
print(power)


set.seed(123)  # for reproducibility

# Define function to run t-test and return p-value
run_t_test <- function() {
  # Draw a sample
  sample <- rnorm(sample_size, mean = mu, sd = sigma)
  
  # Run a t-test and return the p-value
  t_test <- t.test(sample, mu = 0, alternative = "two.sided", conf.level = 0.95)
  return(t_test$p.value)
}

# Replicate the t-test many times
num_iterations <- 1000
p_values <- replicate(num_iterations, run_t_test())

# Count the number of significant results (true positives)
num_true_positives <- sum(p_values < 0.05)

# Calculate the power
power <- num_true_positives / num_iterations

# Print the power
print(power)

set.seed(123)  # for reproducibility

# Define function to run t-test and calculate power
calculate_power <- function(mu, sigma, sample_size, num_iterations=1000) {
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

# Set parameter ranges
sigma_values <- seq(0.1, 2, by = 0.1)
mu_values <- seq(-2, 2, by = 0.1)
sample_sizes <- seq(5, 50, by = 5)

# Calculate power for varying sigma
power_vs_sigma <- sapply(sigma_values, function(sigma) calculate_power(mu = 0.5, sigma = sigma, sample_size = 20))

# Calculate power for varying mu
power_vs_mu <- sapply(mu_values, function(mu) calculate_power(mu = mu, sigma = 1, sample_size = 20))

# Calculate power for varying sample size
power_vs_sample_size <- sapply(sample_sizes, function(sample_size) calculate_power(mu = 0.5, sigma = 1, sample_size = sample_size))

# Plot power vs sigma
plot(sigma_values, power_vs_sigma, main = "Power vs Sigma", xlab = "Sigma", ylab = "Power", type = "l")

# Plot power vs mu
plot(mu_values, power_vs_mu, main = "Power vs Mu", xlab = "Mu", ylab = "Power", type = "l")

# Plot power vs sample size
plot(sample_sizes, power_vs_sample_size, main = "Power vs Sample Size", xlab = "Sample Size", ylab = "Power", type = "l")


# Create a color palette that goes from red to green
color_palette <- colorRampPalette(brewer.pal(9, "RdBu"))(100)

# Reverse the color palette so that low values are red and high values are blue
color_palette <- rev(color_palette)

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
      xlab = "Mu", ylab = "Sigma", main = "Power vs (Mu, Sigma)",
      #col = heat.colors(10))
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
      xlab = "Mu", ylab = "Sample Size", main = "Power vs (Mu, Sample Size)",
       col = viridis(100))

# Add contour lines to the plot to make it easier to read
contour(x = mu_values, y = sigma_values, z = power_matrix, add = TRUE)


