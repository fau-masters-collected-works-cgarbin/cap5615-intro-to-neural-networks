# CAP-5615 Intro to Neural Networks
# Summer 2018
# Homework 3 question 8
# For datasets (Class1.txt and Class2.txt) in question 6, please write a
# Gradient Descent Learning algorithm to find the decision surface to
# separate the two classes.

rm(list = ls()) # start with a clean environment

set.seed(123) # to get repeatable results with random numbers

setwd("~/fau/cap5615/homework3")

instances_per_file = 100

class1 <- read.table("Class1.txt", header = TRUE, sep = ",")
class1.label <- rep(1, instances_per_file)
class1 <- cbind(class1, class1.label)
names(class1) <- c("weight", "height", "label")

class2 <- read.table("Class2.txt", header = TRUE, sep = ",")
class2.label <- rep(-1, instances_per_file) ########################### -1
class2 <- cbind(class2, class2.label)
names(class2) <- c("weight", "height", "label")

class1.2 <- rbind(class1, class2)

network_error <- function(training_data, weights) {
  label_index <- length(training_data[1, ])
  features <- training_data[, -label_index] # includes bias
  labels <- training_data[, label_index]
  
  error <- 0
  for (instance in 1:nrow(training_data)) {
    d <- as.numeric(labels[instance])
    o <- sum(weights * as.numeric(features[instance, ]))
    error <- error + ((d - o)^2)/2
    # debug code
    # print("    _______________")
    # print(paste(c("             d: ", d, collapse=" ")))
    # print(paste(c("             o: ", o, collapse=" ")))
    # print(paste(c("    (d - o)^2): ", (d - o)^2), collapse=" "))
    # print(paste(c("         error: ", error, collapse=" ")))
  }
  return(error)
}

gradient_descent <- function(training_data, eta, error_threshold, niter) {

  # Training data is expected to be [bias=1, feature 1, .., feature m, label]
  number_features = dim(training_data)[2] - 1
  label_index <- length(training_data[1, ])
  features <- training_data[, -label_index] # includes bias
  labels <- training_data[, label_index]
  
  # Error values during the iterations, so we can plot them later
  errors <- rep(error_threshold, niter) # mininum error value is the threshold
  
  # Overall weight value (starts with random values, so any number will do)
  weights <- rep(0.1, number_features)
  #weights <- c(1,0,0)
  
  # Initial network error
  current_error <- network_error(training_data, weights)
  
  # Loop until network error is below thresold or reached the max # iterations
  k <- 1
  while (current_error > error_threshold & k <= niter) {
    # Loop through each instance of training data set to update delta weight
    # The goal is to adjust the overall weight values to reduce the network error
    delta_weight <- rep(0, number_features) # starts at zero
    for (instance in 1:nrow(training_data)) {
      # Saved these to avoid multiple conversions and run a bit faster
      features_numeric = as.numeric(features[instance, ])
      label_numeric = as.numeric(labels[instance])

      # Network output using current weight (before adjusting it)
      output = sum(weights * features_numeric)

      # By how much we need to adjust the overall weight (eta*(d(n)-o(n))*xi)
      weight_adjust <- eta * (label_numeric - output) * features_numeric
      delta_weight <- delta_weight + weight_adjust
    }
    
    # Adjust overall weight and calculate new network error
    weights <- weights + delta_weight
    current_error <- network_error(training_data, weights)
    errors[k] = current_error
    # debug code
    # print(paste(c("weights after: ", weights), collapse=" "))
    # print(paste(c("error after: ", current_error), collapse=" "))

    k <- k + 1
  }

  print(weights)
  print(errors)
  return(list(v1 = weights, v2 = errors))
}

d.set <- data.frame(cbind(rep(1, 2*instances_per_file), class1.2))
names(d.set) <- c("bias", "weight", "height", "label")

samples <- sample(nrow(d.set))
randomized.set <- d.set[samples, ]

# Show sample data
plot(d.set[1:instances_per_file, ]$weight, d.set[1:instances_per_file, ]$height, xlim = c(0:1), ylim = c(0:1), col = "red")
points(d.set[(instances_per_file+1):(2*instances_per_file), ]$weight, d.set[(instances_per_file+1):(2*instances_per_file), ]$height, col = "blue")

# Run training rule
iterations <- 2000

# Note: not the requested 0.05 value
# With 0.05 the network error doesn't converge; it grows instead
# Perhaps we overshot the minimum with that value?
learning_rate <- 0.005

error_threshold <- 0.1
weight.err <- gradient_descent(randomized.set, learning_rate, error_threshold, iterations)
View(weight.err$v2)

# Show training error count
plot(1:iterations, weight.err$v2)
# Show training error rate
error_rate <- weight.err$v2 / 2*instances_per_file
plot(1:iterations, error_rate, xlab = "Iteration number", ylab = "Error rate", yaxt = "n")
axis(2, at = pretty(error_rate), lab = paste0(pretty(error_rate) * 100, "%"), las = TRUE)

plot(d.set[1:instances_per_file, ]$weight, d.set[1:instances_per_file, ]$height, xlim = c(0:1), ylim = c(0:1), col = "red")
points(d.set[(instances_per_file+1):(2*instances_per_file), ]$weight, d.set[(instances_per_file+1):(2*instances_per_file), ]$height, col = "blue")
slope <- weight.err$v1[2] / weight.err$v1[3] * (-1)
intercept <- weight.err$v1[1] / weight.err$v1[3] * (-1)
abline(intercept, slope, col = "green", lty = 2)
