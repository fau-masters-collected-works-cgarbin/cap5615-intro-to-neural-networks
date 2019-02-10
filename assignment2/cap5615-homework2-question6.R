# CAP-5615 Intro to Neural Networks
# Summer 2018
# Homework 2 question 6
# Please use all instances from mtcars.header.binary.categorical.txt to train
# a Naïve Bayes classifier, and use the classifier to predict all instances
# in mtcars.header.binary.categorical.txt, and report the classification
# accuracy.
# Please report the conditional probabilities for attributes “cyl” and “gear”,
# and explain the meaning of the conditional probability values.

# Load data and check its contents
setwd("~/fau/cap5615/assignment2")
cars <- read.table("mtcars.header.binary.categorical.txt", header = TRUE, sep = ",")
head(cars)
summary(cars)
str(cars)

# Create NB classifier
# install.packages("e1071")
require(e1071)
cars_nb <- naiveBayes(factor(mpg) ~ ., data = cars)
cars_nb

# Generate predictions using the sample set (unmodified)
p <- predict(cars_nb, newdata = cars)

# Show confusion matrix for predicted data to check accuracy
table(p, cars$mpg)

# Show conditional probability for cyl and gear
cars_nb$tables$cyl
cars_nb$tables$gear

