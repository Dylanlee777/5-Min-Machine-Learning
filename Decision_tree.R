# 5-min ML episode 1
# R code for making a decision tree by "party" and "rpart" packages
# Test the sensitivity of the decision boundary to the input data
# Written by S.L. June 2020

library(party)
head(iris)
dim(iris)

# Decision tree
tree1 <- ctree(
  Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
  data = iris)
plot(tree1)

#####################################################################################
# Using rpart package
library(rpart)
library(rpart.plot)

tree2 <- rpart(Species ~., data = iris)
rpart.plot(tree2) 

#####################################################################################
# Sensitivity to input data
# custom function "decisionplot" by Michael Hahsler
# URL: https://michael.hahsler.net/SMU/EMIS7332/R/viz_classifier.html

decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}

x <- iris[, c("Sepal.Length", "Sepal.Width", "Species")]
tree3 <- rpart(Species ~ ., data=x)
decisionplot(tree3, x, class = "Species", main = "Original iris Dataset")

x2<-x
x2[54,3]="setosa"
all.equal(x,x2)
tree4 <- rpart(Species ~ ., data=x2)
decisionplot(tree4, x2, class = "Species", main = "Altered iris Dataset")

# package version
sessionInfo()
# R version 4.0.1 (2020-06-06)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# tree_1.0-40       ggplot2_3.3.1     party_1.3-4       strucchange_1.5-2 sandwich_2.5-1   
# zoo_1.8-8         modeltools_0.2-23 mvtnorm_1.1-1     rpart.plot_3.0.8  rpart_4.1-15     