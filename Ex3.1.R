setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(readxl)
library(dplyr)

df <- read_excel("rats.xlsx")


# Mean navigation time
mean_nav_time <- mean(df$Time, na.rm = TRUE)  # 'na.rm = TRUE' removes any missing values before calculation

# Standard deviation and sample size
std_dev <- sd(df$Time)
sample_size <- length(df$Time)  # Excludes missing values

# 95% Confidence interval
error_margin <- qt(0.975, df=sample_size-1) * std_dev / sqrt(sample_size)
lower_bound <- mean_nav_time - error_margin
upper_bound <- mean_nav_time + error_margin

# Output the mean and confidence interval
mean_nav_time
lower_bound
upper_bound


t_test <- t.test(df$Time, mu = 60, alternative = "two.sided", conf.level = 0.95)
t_test
