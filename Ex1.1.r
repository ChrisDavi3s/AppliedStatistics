library(ggplot2)
library(datasets)
library(dplyr)

# Create a data frame from the tree_ring variable
data <- data.frame(tree_ring = treering)

# Generate a histogram of the tree ring data
ggplot(data, aes(x = tree_ring)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +  # Specify histogram details
  labs(# Add labels to the plot
       x = "Normalised Tree Ring Width (dimensionless units)",
       y = "Frequency") +
  theme_minimal()  # Apply minimal theme

# Calculate and display basic statistical measures of the tree ring data
mean(treering)
median(treering)
sd(treering)
IQR(treering)

# Define a coefficient for outlier detection
coef <- 1.5

# Calculate the lower and upper quartiles of the tree ring width data
lower_quantile <- quantile(treering_df$width, 0.25)
upper_quantile <- quantile(treering_df$width, 0.75)

# Calculate the interquartile range (iqr)
iqr <- upper_quantile - lower_quantile

# Identify outliers in the tree ring data using the IQR method
outliers <- treering_df %>%
  filter(width < lower_quantile - coef * iqr | width > upper_quantile + coef * iqr)

#Plot Tree Ring Widths in Methusulah Walk, California
ggplot(treering_df, aes(x = factor(1), y = width)) + #Nastly hack to get x axis to be one variable and not cont.
  stat_boxplot(geom = "errorbar", width=0.25) +
  geom_boxplot(fill = "grey",width=0.5, color = "black", outlier.shape = NA) +
  geom_jitter(data = outliers, width = 0.05, alpha = 0.7, size = 0.5) +  # Add back outliers with jitter
  labs(x = "Tree Rings",
       y = "Width (dimensionless units)") +
  theme_minimal() +
  theme(axis.text.x = element_blank())  #Remove the x axis number



