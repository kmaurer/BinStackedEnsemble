# Load libraries
library(tidyverse)
library(MASS)
library(glmnet)

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







