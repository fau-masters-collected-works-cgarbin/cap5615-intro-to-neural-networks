# CAP-5615 Intro to Neural Networks
# Summer 2018
# Homework 1 question 6 part 3
# Draw scatterplots to show relationship between each attribute and Medv,
# respectively

# Load data and check its contents
setwd("~/fau/cap5615")
housing <- read.table("./assignment1/housing.header.txt", header = TRUE, sep = ",")
head(housing)
summary(housing)
str(housing)

# Plot each relationship to Medv
# There are more elegant ways to do this with ggplot2 and lattice xyplot,
# but that's for another day...
plot(housing$Medv, housing$Crim)
plot(housing$Medv, housing$Zn)
plot(housing$Medv, housing$Indus)
plot(housing$Medv, housing$Chas)
plot(housing$Medv, housing$Nox)
plot(housing$Medv, housing$Rm)
plot(housing$Medv, housing$Age)
plot(housing$Medv, housing$Dis)
plot(housing$Medv, housing$Rad)
plot(housing$Medv, housing$Tax)
plot(housing$Medv, housing$Ptratio)
plot(housing$Medv, housing$B)
plot(housing$Medv, housing$Lstat)
