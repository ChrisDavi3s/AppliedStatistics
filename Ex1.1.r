library(ggplot2)
library(datasets)

hist(treering, 
     main = "Tree Ring Widths in Methusulah Walk, California", 
     xlab = "Tree Ring Width (dimensionless units)", 
     ylab = "Frequency", 
     col = "white", 
     border = "black", 
     breaks = 30)

data <- data.frame(tree_ring = treering)

ggplot(data, aes(x = tree_ring)) +
  geom_histogram(binwidth = 0.1, fill = "gray", color = "black") +
  labs(title = "Tree Ring Widths in Methusulah Walk, California",
       x = "Normalised Tree Ring Width (dimensionless units)",
       y = "Frequency") +
  theme_minimal()

mean(treering)
median(treering)
sd(treering)
IQR(treering)

boxplot(treering, 
        main = "Tree Ring Widths in Methusulah Walk, California", 
        xlab = "Tree Rings", 
        ylab = "Width (dimensionless units)", 
        col = "gray", 
        border = "black")

treering_df <- data.frame(width = as.vector(treering))

ggplot(treering_df, aes(x = 1, y = width)) +
  stat_boxplot(geom = "errorbar", width=0.25) +
  geom_boxplot(fill = "gray", color = "black") +
  geom_jitter(width = 0.1, alpha = 0.5, size = 1) +  # Add jitter
  labs(title = "Tree Ring Widths in Methusulah Walk, California",
       x = "Tree Rings",
       y = "Width (dimensionless units)") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

library(dplyr)

coef <- 1.5
lower_quantile <- quantile(treering_df$width, 0.25)
upper_quantile <- quantile(treering_df$width, 0.75)
iqr <- upper_quantile - lower_quantile

outliers <- treering_df %>%
  filter(width < lower_quantile - coef * iqr | width > upper_quantile + coef * iqr)



ggplot(treering_df, aes(x = 1, y = width)) +
  stat_boxplot(geom = "errorbar", width=0.25) +
  geom_boxplot(fill = "grey",width=0.5, color = "black", outlier.shape = NA) +
  geom_jitter(data = outliers, width = 0.05, alpha = 1, size = 1) +  # Add jitter for outliers
  labs(title = "Tree Ring Widths in Methusulah Walk, California",
       x = "Tree Rings",
       y = "Width (dimensionless units)") +
  theme_minimal()
  
