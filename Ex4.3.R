library(ggplot2)

x <- seq(0,9)

m <- 2
c <- 3
y <- m*x + c


s <- 1
y <- y + rnorm(length(x), mean = 0, sd = s)


model <- lm(y ~ x)
summary(model)
plot(x, y)
abline(model, col="red") # adds a regression line

df <- data.frame(x=x, y=y) # create a dataframe to use with ggplot2

ggplot(df, aes(x=x, y=y)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color="red") + 
  labs(title="Scatterplot with regression line", x="x", y="y")

y[5] <- y[5] + 10 # Add an outlier
model <- lm(y ~ x)
summary(model)

# Plotting again
df$y <- y
ggplot(df, aes(x=x, y=y)) + 
  geom_point() +
  geom_smooth(method=lm, se=FALSE, color="red") +
  labs(title="Scatterplot with regression line (with outlier)", x="x", y="y")

