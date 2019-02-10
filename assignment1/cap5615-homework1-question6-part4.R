# CAP-5615 Intro to Neural Networks
# Summer 2018
# Homework 1 question 6 part 4
# Explain how to use scatterplots to find attributes which are positively
# correlated, negatively correlated, or independent of Medv, respectively

# Load data and check its contents
setwd("~/fau/cap5615")
housing <- read.table("./assignment1/housing.header.txt", header = TRUE, sep = ",")
head(housing)
summary(housing)
str(housing)

# We already know from the previous part of this question that Rm and Lstat
# have linear relationships with Medv
# This plots a fitting line to show the relationship

# Positively correlated
plot(housing$Medv, housing$Rm)
lines(lowess(housing$Medv, housing$Rm))

# Negatively correlated
plot(housing$Medv, housing$Lstat)
lines(lowess(housing$Medv, housing$Lstat))
