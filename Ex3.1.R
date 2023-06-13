# Required libraries
library(readxl)
library(dplyr)

#set working directory to current editor context
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load the data from the Excel file
rat_data <- read_excel("rats.xlsx")

# Compute the mean navigation time, excluding missing values
mean_navigation_time <- mean(rat_data$Time, na.rm = TRUE)

# Compute the standard deviation, excluding missing values
navigation_time_std_dev <- sd(rat_data$Time, na.rm = TRUE)

# Compute the sample size, excluding missing values
sample_size <- sum(!is.na(rat_data$Time))

# Compute the error margin for a 95% confidence interval
error_margin <- qt(0.975, df=sample_size-1) * navigation_time_std_dev / sqrt(sample_size)

# Compute the lower and upper bounds of the 95% confidence interval
confidence_interval_lower_bound <- mean_navigation_time - error_margin
confidence_interval_upper_bound <- mean_navigation_time + error_margin

# Output the mean and confidence interval
print(paste0("Mean navigation time: ", mean_navigation_time))
print(paste0("95% Confidence Interval: [", confidence_interval_lower_bound, ", ", confidence_interval_upper_bound, "]"))

# Perform a two-sided t-test comparing the sample mean to a hypothesized population mean of 60, with a confidence level of 95%
t_test_result <- t.test(rat_data$Time, mu = 60, alternative = "two.sided", conf.level = 0.95)

# Print the t-test results
print(t_test_result)

