# CAP-5615 Intro to Neural Networks
# Summer 2018
# Homework 3 question 8 - v2
# For datasets (Class1.txt and Class2.txt) in question 6, please write a
# Gradient Descent Learning algorithm to find the decision surface to
# separate the two classes.

rm(list = ls()) # start with a clean environment
set.seed(123) # to get repeatable results with random numbers
setwd("~/fau/cap5615/homework3")

network_error <- function(training_data, weights) {
  label_index <- length(training_data[1, ])
  features <- training_data[, -label_index] # includes bias
  labels <- training_data[, label_index]

  error <- 0
  for (instance in 1:nrow(training_data)) {
    d <- as.numeric(labels[instance])
    o <- sum(weights * as.numeric(features[instance, ]))
    error <- error + ((d - o)^2) / 2
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
  number_features <- dim(training_data)[2] - 1
  label_index <- length(training_data[1, ])
  features <- training_data[, -label_index] # includes bias
  labels <- training_data[, label_index]

  # Overall weight value (starts with random values, so any number will do)
  weights <- rep(0.1, number_features)

  # Initial network error
  current_error <- network_error(training_data, weights)

  # Network error values for each iteration
  errors <- rep(error_threshold, niter) # mininum error value is the threshold

  # Loop until network error is below threshold or max # iterations reached
  k <- 1
  while (current_error > error_threshold & k <= niter) {
    # Loop through each instance of training data set to update delta weight
    # The goal is to adjust the overall weight values to reduce the network error
    delta_weight <- rep(0, number_features) # starts at zero for each instance
    for (instance in 1:nrow(training_data)) {
      # Save values we use more than once to avoid multiple conversions and run a bit faster
      features_numeric <- as.numeric(features[instance, ])

      # Network output using current weight: o(n)
      o <- sum(weights * features_numeric)

      # By how much we need to adjust the overall weight (eta*(d(n)-o(n))*xi)
      weight_adjust <- eta * (as.numeric(labels[instance]) - o) * features_numeric
      delta_weight <- delta_weight + weight_adjust
    }

    # Adjust overall weight and calculate new network error
    weights <- weights + delta_weight
    current_error <- network_error(training_data, weights)
    # debug code
    # print(paste(c("weights after: ", weights), collapse=" "))
    # print(paste(c("error after: ", current_error), collapse=" "))

    # Accumulate errors seen so far
    errors[k] <- current_error

    k <- k + 1
  }

  return(list(w = weights, e = errors))
}

# Read a file, apply the given label and add column names
read_file <- function(filename, label, instances) {
  f <- read.table(filename, header = TRUE, sep = ",")
  f.label <- rep(label, instances)
  f <- cbind(f, f.label)
  names(f) <- c("weight", "height", "label")
  return(f)
}

# Read input data, add class (label) and bias
instances_per_file <- 100
class1 <- read_file("Class1.txt", 1, instances_per_file)
class2 <- read_file("Class2.txt", -1, instances_per_file)

# Split into training (80%) and test (20%) sets
# Note that we will use the same indices for traning for class1 and class 2
# to simplify the code a bit
train_size <- floor(0.8 * nrow(class1)) # number of rows to use for training (each class)
train_ind <- sample(seq_len(nrow(class1)), size = train_size)
class1_train <- class1[train_ind, ]
class1_test <- class1[-train_ind, ]
class2_train <- class2[train_ind, ]
class2_test <- class2[-train_ind, ]

# Combine into one training and one test data set
class1.2_train <- rbind(class1_train, class2_train)
d_train.set <- data.frame(cbind(rep(1, 2 * train_size), class1.2_train))
names(d_train.set) <- c("bias", "weight", "height", "label")
samples_train <- sample(nrow(d_train.set))
randomized_train.set <- d_train.set[samples_train, ]

test_size <- nrow(class1) - train_size # each class
class1.2_test <- rbind(class1_test, class2_test)
d_test.set <- data.frame(cbind(rep(1, 2 * test_size), class1.2_test))
names(d_test.set) <- c("bias", "weight", "height", "label")
samples_test <- sample(nrow(d_test.set))
randomized_test.set <- d_test.set[samples_test, ]

# Train the classifier

# Note: not the requested 0.05 value
# With 0.05 the network error doesn't converge; it grows instead
# Perhaps we overshot the minimum with that value?
learning_rate <- 0.005
iterations <- 2000
error_threshold <- 0.1
train_result <- gradient_descent(randomized_train.set, learning_rate, error_threshold, iterations)
weights <- train_result$w
print(train_result$w)
print(train_result$e)

# Classify the test data using the weigth from the training data
# The activation functions: class = 1 if v >= 0; class = -1 otherwise
label_index <- length(randomized_test.set[1, ])
features_test <- randomized_test.set[, -label_index] # includes bias
labels_test <- randomized_test.set[, label_index]
output_test <- rep(0, test_size)
for (i in 1:(test_size * 2)) {
  o <- sum(weights * as.numeric(features_test[i, ]))
  if (o >= 0) {
    output_test[i] <- 1
  }
  else {
    output_test[i] <- -1
  }
}

# Accuracy
ctable <- table(output_test, labels_test) # confusion table
ctable
TN <- ctable[1, 1]
FP <- ctable[1, 2]
FN <- ctable[2, 1]
TP <- ctable[2, 2]
accuracy <- (TN + TP) / (TN + FP + FN + TP)

# Graph training error and test error count
test_result <- gradient_descent(
  randomized_test.set, learning_rate,
  error_threshold, iterations
)
print(test_result$w)
print(test_result$e)
plot(1:iterations, train_result$e, col = "red", ylim = c(10, 80), xlab = "", ylab = "")
par(new = TRUE) # do not clean the frame before new drawing
plot(1:iterations, test_result$e,
  col = "blue", ylim = c(10, 80),
  xlab = "Iteration number", ylab = "Error count (red=training, blue=test)"
)

# Graph training and test data together
class1.2 <- rbind(class1, class2)
d.set <- data.frame(cbind(rep(1, 200), class1.2))
names(d.set) <- c("bias", "weight", "height", "label")
plot(d.set[1:instances_per_file, ]$weight, d.set[1:instances_per_file, ]$height, xlim = c(0:1), ylim = c(0:1), col = "red")
points(d.set[(instances_per_file + 1):(2 * instances_per_file), ]$weight, d.set[(instances_per_file + 1):(2 * instances_per_file), ]$height, col = "blue")

# Graph classification surface (hyperplane)
slope <- train_result$w[2] / train_result$w[3] * (-1)
intercept <- train_result$w[1] / train_result$w[3] * (-1)
abline(intercept, slope, col = "green", lty = 2)

