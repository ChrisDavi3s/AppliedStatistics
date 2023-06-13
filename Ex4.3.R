# Function to simulate data, fit a model and return a summary of the model
simulate_and_model <- function(m, c, s, n_iter=10000, error_fn = rnorm, outlier = FALSE) {
  x <- seq(0,9)  # Generate the x values
  
  intercepts <- numeric(n_iter)  # Vector to store intercept values
  slopes <- numeric(n_iter)  # Vector to store slope values
  p_values_intercept <- numeric(n_iter)  # Vector to store intercept p-values
  p_values_slope <- numeric(n_iter)  # Vector to store slope p-values
  
  for(i in 1:n_iter) {
    y <- m*x + c + error_fn(length(x), mean = 0, sd = s)  # Generate y values with error term
    if(outlier) { y[5] <- y[5] + 10 }  # Add an outlier if specified
    
    model <- lm(y ~ x)  # Fit the linear regression model
    intercepts[i] <- coef(model)[1]  # Store the intercept
    slopes[i] <- coef(model)[2]  # Store the slope
    p_values_intercept[i] <- summary(model)$coefficients[1,4]  # Store the intercept p-value
    p_values_slope[i] <- summary(model)$coefficients[2,4]  # Store the slope p-value
  }
  
  # Create a data frame with the summary statistics
  data.frame(Intercept = mean(intercepts), 
             Slope = mean(slopes), 
             Intercept_p_value = mean(p_values_intercept),
             Slope_p_value = mean(p_values_slope))
}

# Run the function for each scenario
res1 <- simulate_and_model(m=1, c=3, s=1)  # Initial parameters scenario
res2 <- simulate_and_model(m=10, c=3, s=1)  # Different Gradient scenario
res3 <- simulate_and_model(m=1, c=3, s=10)  # Different Sigma scenario
res4 <- simulate_and_model(m=1, c=3, s=1, outlier = TRUE)  # With outlier scenario
res5 <- simulate_and_model(m=1, c=3, s=1, error_fn = function(n, mean, sd) rnorm(n, mean, sd + seq(0, 9)*0.5))  # Varying error scenario

# Combine the results
res_all <- rbind(cbind(Scenario = "Initial parameters", res1), 
                 cbind(Scenario = "Different Gradient", res2), 
                 cbind(Scenario = "Different Sigma", res3), 
                 cbind(Scenario = "With outlier", res4),
                 cbind(Scenario = "Varying error", res5))

print(res_all)  # Print the combined results
