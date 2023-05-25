library(ggplot2)

sample_size <- 100
num_samples <- 1000

sample_means <- replicate(num_samples, {
  cauchy_sample <- rcauchy(sample_size)
  mean(cauchy_sample)
})

cauchy_df <- data.frame(sample_means)

ggplot(cauchy_df, aes(sample = sample_means)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Sample Means (Cauchy Distribution)",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")


sample_size <- 100
num_samples <- 1000

sample_means <- replicate(num_samples, {
  cauchy_sample <- rcauchy(sample_size)
  mean(cauchy_sample)
})

qqnorm(sample_means, main = "Q-Q Plot of Sample Means (Cauchy Distribution)",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(sample_means, col = "red")


