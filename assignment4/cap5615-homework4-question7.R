# CAP-5615 Intro to Neural Networks
# Summer 2018
# Homework 4 question 7
# Please randomly select 80% of images as training samples to train neural networks with
# one hidden layer but different number of hidden nodes (3, 5, 7, 9, 11 hidden nodes,
# respectively). Please show the classification accuracies of the neural networks on the
# remaining 20% of images (which are not selected as training samples). Please report R
# code [2 pt], and also use a table to summarize the classification accuracy of the neural
# networks with respect to different number of hidden nodes [1 pt].
# [Images from the CMU Face Recognition website (http://www.cs.cmu.edu/~tom/faces.html)]

# install.packages("pixmap")
library(pixmap)
library(gdata)

rm(list = ls()) # start fresh to avoid subtle bugs
set.seed(123) # to get repeatable results with random numbers

image_path <- "~/fau/cap5615/assignment4/images"
setwd(image_path)

# Show an example of each class (right=1, left=0)
class1_sample <- read.pnm(file = "an2i_right_sad_open_4.pgm", cellres = 1)
plot(class1_sample)
class0_sample <- read.pnm(file = "saavik_left_happy_open_4.pgm", cellres = 1)
plot(class0_sample)

# Reads image files matching pattern, add class label and return them as grey-scale data frame,
# with the given as class as the last element in the vectors.
# Returns two data frames in a list: 80% of images as training data and 20% as test data.
loadImages <- function(pathName, filePattern, classLabel) {
  # Get all file names matching the pattern
  files <- list.files(path = pathName, pattern = filePattern, all.files = T, full.names = F, no.. = T)
  # Read all images
  images <- lapply(files, read.pnm, cellres = 1)
  # Debug: show the first image
  # plot(images[[1]])

  # Transform image to grey scale vectors
  image.matrix <- images[[1]]@grey
  image.vector <- unmatrix(image.matrix, byrow = T)
  for (ii in 2:length(images)) {
    i.matrix <- images[[ii]]@grey
    i.vector <- unmatrix(i.matrix, byrow = T)
    image.vector <- rbind(image.vector, i.vector)
  }

  # Change to a data frame
  image.frame <- data.frame(image.vector)

  # Add class label
  number_of_images <- nrow(image.frame)
  class_label <- rep(classLabel, number_of_images)
  image.frame <- cbind(image.frame, class_label)

  # Split into training (80%) and test (20%) sets
  train_size <- floor(0.8 * number_of_images)
  train_ind <- sample(seq_len(number_of_images), size = train_size)
  training_set <- image.frame[train_ind, ]
  test_set <- image.frame[-train_ind, ]

  return(list(training = training_set, test = test_set))
}

# Creates a neural net with the given number of hidden nodes, trains and tests it.
# Returns the predicted class using the test data (continous values).
classify_faces <- function(training, test, hidden_layers) {
  # install.packages("neuralnet")
  library(neuralnet)

  myform <- as.formula(paste("class_label ~ ", paste(names(training[!names(training) %in% "class_label"]),
    collapse = " + "
  )))
  classifier <- neuralnet(myform, training, hidden = hidden_layers, rep = 100, linear.output = FALSE, threshold = 0.1)

  class_index <- length(test)
  predicted <- compute(classifier, test[, -class_index])

  return(predicted$net.result)
}

show_confusion_matrix <- function(title, test, classification) {
  # Changes from continuous to discrete values (must match the data labels)
  # Using 0.5 as the cutoff point to classify between the two labels
  predicted <- cut(classification, breaks = c(-Inf, 0.5, Inf), labels = c(0L, 1L))

  # Extract the actual labels (assumes label is the last column in the test set)
  class_index <- length(test)
  actual <- test[, class_index]

  # Show confusion matrix and accuracy
  t <- table(predicted, actual)
  TN <- t[1, 1]
  FP <- t[1, 2]
  FN <- t[2, 1]
  TP <- t[2, 2]
  accuracy <- (TN + TP) / (TN + FP + FN + TP)

  # Debug code
  # print(predicted)
  # print(actual)

  print(title)
  print(t)
  print(accuracy)
}

class1_all <- loadImages(image_path, ".*right.*\\.pgm", 1)
class0_all <- loadImages(image_path, ".*left.*\\.pgm", 0)
training_set <- rbind(class1_all$training, class0_all$training)
test_set <- rbind(class1_all$test, class0_all$test)

# Test different number of hidden nodes (3, 5, 7, 9, 11)

classification3 <- classify_faces(training_set, test_set, 3)
show_confusion_matrix("3 hidden nodes", test_set, classification3)

classification5 <- classify_faces(training_set, test_set, 5)
show_confusion_matrix("5 hidden nodes", test_set, classification5)

classification7 <- classify_faces(training_set, test_set, 7)
show_confusion_matrix("7 hidden nodes", test_set, classification7)

classification9 <- classify_faces(training_set, test_set, 9)
show_confusion_matrix("9 hidden nodes", test_set, classification9)

classification11 <- classify_faces(training_set, test_set, 11)
show_confusion_matrix("11 hidden nodes", test_set, classification11)

# Show the predicted values in a scatter plot
plot(classification3, ylab="Predicated value", xlab="Test instance")
points(classification5, pch=21, bg="red")
points(classification7, pch=22, bg="lightblue")
points(classification9, pch=23, bg="lightgreen")
points(classification11, pch=24, bg="lightgrey")

