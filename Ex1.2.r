# Load necessary libraries
library(readxl)  # for reading Excel files
library(dplyr)   # for data manipulation
library(ggplot2) # for data visualization

# Set the working directory to the directory of the current script (ie the current folder)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Read the data from the 'guessinglengths.xlsx' file
guessing_lengths <- read_excel("guessinglengths.xlsx")

# Filter the dataset for 'metres' and calculate the median of 'guess' column
median_metres <- median(filter(guessing_lengths, unit == "metres")$guess)

# Filter the dataset for 'feet' and calculate the median of 'guess' column
median_feet <- median(filter(guessing_lengths, unit == "feet")$guess)

# Print the median values to the console using the 'cat()' function
cat("Median for guesses in metres:", median_metres, "\n")
cat("Median for guesses in feet:", median_feet, "\n")

# Create a box plot comparing the two groups: Comparison of Lecture Hall Width Guesses
ggplot(data = guessing_lengths, aes(x = unit, y = guess)) +
  stat_boxplot(geom = "errorbar", width=0.25) +
  geom_boxplot(fill = "grey") +
  labs(x = "Unit",
       y = "Width Guess") +
  theme_minimal()
