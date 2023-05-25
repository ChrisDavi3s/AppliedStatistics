library(readxl)
library(dplyr)
library(ggplot2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Read the data from the 'guessinglengths.xlsx' file
guessing_lengths <- read_excel("guessinglengths.xlsx")

# Calculate the medians
median_metres <- median(filter(guessing_lengths, unit == "metres")$guess)
median_feet <- median(filter(guessing_lengths, unit == "feet")$guess)

cat("Median for guesses in metres:", median_metres, "\n")
cat("Median for guesses in feet:", median_feet, "\n")

# Create a box plot comparing the two groups
ggplot(data = guessing_lengths, aes(x = unit, y = guess)) +
  stat_boxplot(geom = "errorbar", width=0.25) +
  geom_boxplot(fill = "grey") +
  labs(title = "Comparison of Lecture Hall Width Guesses",
       x = "Unit",
       y = "Width") +
  theme_minimal()
