# CAP-5615 Intro to Neural Networks
# Summer 2018
# Homework 1 question 7
# Report the pairwise correlation between every two variables (either as a
# matrix or as a level plot)

# Load data and check its contents
setwd("~/fau/cap5615")
housing <- read.table("./assignment1/housing.header.binary.txt", header = TRUE, sep = ",")
head(housing)
summary(housing)
str(housing)

# Create decision tree
library(rpart)
tree <- rpart(Medv ~ Rm + Lstat, method = "class", data = housing)

# Show tree - fancy version with rpart.plot
# See http://www.milbo.org/doc/prp.pdf
library(rpart.plot)
rpart.plot(tree, type = 2, extra = 1)

# Save as PS file in the current working directory
post(tree,
  file = "medv-tree.ps",
  title = "Medv classification based on Lstat and Rm"
)


View(housing)
