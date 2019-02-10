# CAP-5615 Intro to Neural Networks
# Summer 2018
# Homework 1 question 6 part 1
# Report the pairwise correlation between every two variables (either as a
# matrix or as a level plot)

# Load data and check its contents
setwd("~/fau/cap5615")
housing <- read.table("./assignment1/housing.header.txt", header = TRUE, sep = ",")
head(housing)
summary(housing)
str(housing)

# Create correlation data set
housingcor <- cor(housing)

# Heatmap correlation from lattice
# Black-and-white scale courtesy of https://stackoverflow.com/questions/6556530/how-do-i-control-a-heatmap-with-lattice-and-levelplot
# Found that scale easier to read than the standard blue/magenta one
library(lattice)
ramp <- colorRamp(c("white", "black"))
levelplot(housingcor,
  scales = list(x = list(rot = 90)),
  col.regions = rgb(ramp(seq(0, 1, length = 1000)), max = 255)
)

#-----------------------
# Starting at this point: other ways to display the correlation data
# Investigated while doing the homework, but not used because the question
# explicitly asks for a level plot
# Saved here for future reference

# Correlation plot with circles and colors
# Easier to find positive/negative correlations
install.packages("corrplot")
library(corrplot)
corrplot(housingcor,
  type = "upper", order = "hclust",
  tl.col = "black", tl.srt = 45
)

# Standard R heatmap
# Not that great, but doesn't require other libraries
heatmap(housingcor, Rowv = NA, Colv = NA, col = heat.colors(256))

# Spreadsheet-like view of the data
View(housingcor)
