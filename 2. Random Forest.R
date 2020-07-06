#####################################################################################
# 5-min Machine Learning episode 2
# R code for making a random forest with "randomForest" package
# Data used: titanic from Kaggle
# https://www.kaggle.com/c/titanic/data
# Written by S.L. June 2020
#####################################################################################
library(readr)
train <- read.csv("C:/Users/dylan/Desktop/Youtube_ML/2 Random Forest/Data/train.csv")
validation <- read.csv("C:/Users/dylan/Desktop/Youtube_ML/2 Random Forest/Data/test.csv")

# Training data inspection
# Optional
dim(train)
summary(train)
head(train)

####################################################################################################
# Histograms & tables
table(train$Survived)
table(train$Survived,train$Pclass)
table(train$Survived,train$Sex)
# Two-in-one histogram: age
c0 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")
c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")

hg_age0 <- hist(train[train$Survived =="0",]$Age, plot = FALSE) # Save first histogram data
hg_age1 <- hist(train[train$Survived =="1",]$Age, plot = FALSE) # Save second histogram data
plot(hg_age0, col = c0) # Plot 1st histogram using a transparent color
plot(hg_age1, col = c1, add = TRUE) # Add 2nd histogram using different color
legend(60,100,
       legend=c("Not Survived","Survived"),
       fill = c(c0,c1))

table(train$Survived,train$SibSp)
table(train$Survived,train$Parch)

# Two-in-one histogram: Fare
a <- min(train$Fare)  # Set the minimum for the breakpoints
b <- max(c(train$Fare)) # Set the maximum for the breakpoints
ab <- pretty(a:b, n = 20) # Make a neat vector for the breakpoints
hg_fare0 <- hist(train[train$Survived =="0",]$Fare, breaks = ab, plot = FALSE) # Save first histogram data
hg_fare1 <- hist(train[train$Survived =="1",]$Fare, breaks = ab, plot = FALSE) # Save second histogram data
plot(hg_fare0, col = c0) # Plot 1st histogram using a transparent color
plot(hg_fare1, col = c1, add = TRUE) # Add 2nd histogram using different color
legend(400,300,
       legend=c("Not Survived","Survived"),
       fill = c(c0,c1))

table(train$Survived,train$Embarked)
####################################################################################################

# Replance NA with blank
sapply(train,FUN = function(x){
  sum(is.na(x)) 
})

train[is.na(train)] <- ""

# Turn target variable to factor
train$Survived <- as.factor(train$Survived)

# Delete PassengerId
train<-train[,-(names(train)=="PassengerId")]

# Random Forest
library(randomForest)
rf <- randomForest(
  Survived ~ .,
  data = train
)
rf # confusion matrix for training

# Prediction


# Plot the error rate in terms of ntree
plot(rf, type="l", main="Error Rate",log="y")

###################################################################################################
# Feature Importance
rf_importance<-importance(rf,type = 2)
rf_importance[order(-rf_importance[,1]),]

# Plot importance
# Function credit
# https://blog.methodsconsultants.com/posts/be-aware-of-bias-in-rf-variable-importance-metrics/
create_rfplot <- function(rf, type){
  
  imp <- importance(rf, type = type, scale = F)
  
  featureImportance <- data.frame(Feature = row.names(imp), Importance = imp[,1])
  
  p <- ggplot(featureImportance, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "#53cfff", width = 0.65) +
    coord_flip() + 
    theme_light(base_size = 20) +
    theme(axis.title.x = element_text(size = 15, color = "black"),
          axis.title.y = element_blank(),
          axis.text.x  = element_text(size = 15, color = "black"),
          axis.text.y  = element_text(size = 15, color = "black")) 
  return(p)
}
library(ggplot2)
create_rfplot(rf, type = 2)

# package version
sessionInfo()
# R version 4.0.1 (2020-06-06)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# randomForest_4.6-14 COUNT_1.3.4         sandwich_2.5-1      msme_0.5.3          lattice_0.20-41     MASS_7.3-51.6      
