# Animated version of the perceptron rule

setwd("~/fau/cap5615/assignment3")

class1 <- read.table("Class1.txt", header = TRUE, sep = ",")
class1.label <- rep(1, 100)
class1 <- cbind(class1, class1.label)
names(class1) <- c("weight", "height", "label")

class2 <- read.table("Class2.txt", header = TRUE, sep = ",")
class2.label <- rep(-1, 100)
class2 <- cbind(class2, class2.label)
names(class2) <- c("weight", "height", "label")

class1.2 <- rbind(class1, class2)

d.set <- data.frame(cbind(rep(1, 200), class1.2))
names(d.set) <- c("bias", "weight", "height", "label")

samples <- sample(nrow(d.set))
randomized.set <- d.set[samples, ]

iterations <- 500
eta <- 0.05
x <- randomized.set
print("---")
# initialize weight vector
weight <- rep(0.1, dim(x)[2] - 1)
errors <- rep(0, iterations)
label.index <- length(x[1, ])
features <- x[, -label.index]
labels <- x[, label.index]

# loop over number of epochs
for (jj in 1:iterations) {

  # loop through training data set
  for (ii in 1:nrow(x))
  {
    cat(sprintf("Iteration %d, row %d\n", jj, ii))

    # Cache these values to avoid multiple conversions
    features_numeric <- as.numeric(features[ii, ])
    labels_numeric <- as.numeric(labels[ii])

    # Predict binary label using activation function
    z <- sum(weight[1:length(weight)] * features_numeric)
    if (z < 0) {
      ypred <- -1
    } else {
      ypred <- 1
    }

    # Change weight - the formula doesn't do anything
    # if the predicted value is correct
    weightdiff <- eta * (labels_numeric - ypred) * features_numeric
    weight <- weight + weightdiff

    plot(d.set[1:100, ]$weight, d.set[1:100, ]$height, xlim = c(0:1), ylim = c(0:1), col = "red")
    points(d.set[101:200, ]$weight, d.set[101:200, ]$height, col = "blue")

    slope <- weight[2] / weight[3] * (-1)
    intercept <- weight[1] / weight[3] * (-1)
    abline(intercept, slope, col = "green", lty = 2)

    # Important: need this to force RStudio update graphs
    # And even this doesn't guarantee it will update in every loop
    # It will update once in a while, enough to have a rough idea
    Sys.sleep(0.05)

    # update error rate
    if ((labels_numeric - ypred) != 0.0) {
      errors[jj] <- errors[jj] + 1
    }
  }
}

# weight to decide between the two species
print(weight)
print(errors)
