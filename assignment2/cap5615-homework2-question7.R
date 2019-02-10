# CAP-5615 Intro to Neural Networks
# Summer 2018
# Homework 2 question 7
# Please download housing.header.binary.txt dataset from Canvas, and use R to
# implement tasks below (a brief description of this dataset is available
# from the following URL) https://archive.ics.uci.edu/ml/datasets/housing
# [The Medv attribute in housing.header.binary.txt is binarized with Medv
# value of the house greater than 200k being 1, or 0 otherwise. ]

# Load data and check its contents
setwd("~/fau/cap5615/assignment2")
housing <- read.table("housing.header.binary.txt", header = TRUE, sep = ",")
head(housing)
summary(housing)
str(housing)

# Please use 80% of instances in the “housing.header.binary.txt” dataset to
# build a Naïve Bayes Classifier. Report the performance of the NB classifier
# on the remaining 20% of instances in the “housing.header.binary.txt”

# Split into 80% training and 20% test data for the next steps
set.seed(123) # to ensure consistent splitting across runs
train_size <- floor(0.8 * nrow(housing)) # number of rows to use for training
train_ind <- sample(seq_len(nrow(housing)), size = train_size)
housing_train <- housing[train_ind, ]
housing_test <- housing[-train_ind, ]

# install.packages("e1071")
library(e1071)
housing_nb <- naiveBayes(factor(Medv) ~ ., data = housing_train)

# Report confusion table, TPR, FPR, and the Accuracy
# We need the predicted classes for that
predict_class <- predict(housing_nb, newdata = housing_test, type = "class")
ctable <- table(predict_class, housing_test$Medv) # confusion table
ctable
TN <- ctable[1, 1]
FP <- ctable[1, 2]
FN <- ctable[2, 1]
TP <- ctable[2, 2]
accuracy <- (TN + TP) / (TN + FP + FN + TP)
TPR <- TP / (TP + FN)
FPR <- FP / (FP + TN)

# Report the ROC curve
# We need the predicted probabilities for that
predict_raw <- predict(housing_nb, newdata = housing_test, type = "raw")
predict_raw[, 2]
predict_raw_class <- cbind(predict_raw[, 2], housing_test$Medv)
predict_raw_class

# install.packages("ROCR")
library(ROCR)
pred <- prediction(predict_raw_class[, 1], predict_raw_class[, 2])
perf <- performance(pred, "tpr", "fpr")
plot(perf, col = "red")
abline(0, 1, col = "lightgray")

# Report the AUC value
auc <- performance(pred, "auc")
auc
auc@y.values[[1]]

# Create a new instance with “Crim=0.03, Zn=13, Indus=3.5, Chas=0.3, Nox=0.58,
# Rm=4.1, Age=68, Dis=4.98, Rad =3, Tax=225, Ptratio=17, B=396, Lstat=7.56”, ...
new_inst <- data.frame(
  "Crim" = 0.03, "Zn" = 13, "Indus" = 3.5, "Chas" = 0.3,
  "Nox" = 0.58, "Rm" = 4.1, "Age" = 68, "Dis" = 4.98, "Rad" = 3, "Tax" = 225,
  "Ptratio" = 17, "B" = 396, "Lstat" = 7.56
)
# ...and predict the Medv value of the instance. Report the posterior probability,
# and the classification result.
predict(housing_nb, newdata = new_inst, type = "raw") # probability
predict(housing_nb, newdata = new_inst, type = "class") # classification
