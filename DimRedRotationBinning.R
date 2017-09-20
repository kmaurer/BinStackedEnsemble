# Load libraries
library(tidyverse)
library(MASS)
library(glmnet)
library(psych)


# Read Data from book website
college <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/College.csv")
college <- dplyr::select(college, -X, -Private)
head(college)
dim(college)
str(college)

#-----------------------------------------------------------------
### Principle Component Regression

### We can start by using the Principal Component Analysis functions from the psych package
# This function has better options for viewing the selected phi's and lambda's
library(psych)
?principal
# rotate X based on eigenvalues/eigenvectors of correlation (scaled PCA) or covariance (unscaled PCA)
cor(college[,-17])
mypca <- principal(cor(college[,-17]), nfactors=16)
# Phi Matrix pXp made from eigenvectors (no dim reduction, just rotation)
mypca$weights
# eigenvalues letting us know how much of var[X] is represented in each PC
mypca$values
# How much of the total variability in the X-space 
# is explained through each principle component?
plot(1:16, mypca$values/16) #"screeplot"
plot(1:16, cumsum(mypca$values)/16) #cumulative variance plot

## Try it out with manual coding for rotation->reduction->regression
# rotation
xmatrix <- as.matrix(college[,-17])
head(xmatrix)
phimatrix <- mypca$rot.mat
head(phimatrix)
zmatrix <- xmatrix %*% phimatrix
head(zmatrix)
# dimension reduction by dropping columns of phimatrix before rotation
zreduced <- xmatrix %*% phimatrix[,1:9]
head(zreduced)

dim_reduced_data <- data.frame(Grad.Rate=college$Grad.Rate,
                               zreduced)
head(dim_reduced_data)

#-------------------------------------------------------------------------------------------------------------

library(data.table)
abalone <- fread('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data')
abalone<-as.data.frame(abalone)
names(abalone)[1]<-"true_class"
abalone$true_class<-as.factor(abalone$true_class)
true_classes <- levels(abalone$true_class)
model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO")


abalone_bin_fitted <- cv_testing_all_ensembles(data=abalone,model_types,bin_features_all=c("V2","V3"), nbins_all=2:4,
                                                          equal_bins=TRUE, cv_K=20)
head(abalone_bin_fitted)

mypca <- principal(cor(abalone[,-1]), nfactors=8)
phimatrix <- mypca$rot.mat
plot(1:8, mypca$values/8) #"screeplot"
plot(1:8, cumsum(mypca$values)/8) #cumulative variance plot

abalone_rotated <- data.frame(true_class=abalone$true_class, as.data.frame(as.matrix(abalone[,-1]) %*% phimatrix))
head(abalone_rotated)

abalone_rotated_bin_fitted <- cv_testing_all_ensembles(data=abalone_rotated,model_types,bin_features_all=c("V1","V2"), nbins_all=2:4,
                                               equal_bins=TRUE, cv_K=20)
head(abalone_rotated_bin_fitted)

abalone_bin_fitted$rect_binned_results$accuracy - 
abalone_rotated_bin_fitted$rect_binned_results$accuracy

abalone_bin_fitted$iq_binned_results$accuracy 
abalone_rotated_bin_fitted$iq_binned_results$accuracy
