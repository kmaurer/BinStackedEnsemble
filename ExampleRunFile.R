setwd("~/GitHub/BinStackedEnsemble/R")
source("binningFunctionsUpdated.R")
source("predictFunctionsUpdated.R")
source("trainFunctionsUpdated.R")
source("weightingFunctionsUpdated.R")

# source experimental Iterative Quantile binning functions
source("C:\\Users\\maurerkt\\Documents\\GitHub\\IQbinR\\IterativeQuantileBinningSupportFunctions.R")
source("C:\\Users\\maurerkt\\Documents\\GitHub\\IQbinR\\IterativeQuantileBinning.R")
#--------------------------------------------------------------------------
# Iris example
##
names(iris)[5] <- "true_class"
train_index <- c(sample(1:50, 30),sample(51:100, 30),sample(101:150, 30))
train <- iris[train_index, ]
test <- iris[-train_index, ]


### Build models to stack
# -------
# ## Manual building to allow tuning specification
# naiveBayes <- RWeka::make_Weka_classifier("weka.classifiers.bayes.NaiveBayes")
# model1 <- naiveBayes(true_class ~., train)
# 
# randomForest <- RWeka::make_Weka_classifier("weka.classifiers.trees.RandomForest")
# model2 <- randomForest(true_class ~., train)
# 
# bagging <- RWeka::make_Weka_classifier("weka.classifiers.meta.Bagging")
# model3 <- bagging(true_class ~., train)
# 
# svm <- RWeka::make_Weka_classifier("weka.classifiers.functions.SMO")
# model4 <- svm(true_class ~., train)
# 
# modelList <- list(model1, model2, model3, model4)

# -------
## Specify member classifiers with function
model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO")

modelList <- make_model_list(model_types, test)
modelList[[1]]
predict(modelList[[1]], test)
predict(modelList[[1]], test, type="probability")


# -------
## Specify combination rules and binning types
# weightType <- "bin weighted"
weightType <- "bin dictator"
comb_rule <- "majority vote"
# comb_rule <- "average posterior"
# bin_type <- "quantile"
bin_type <- "standard"
# bin_type <- "iterative quantile"
bin_features <- c("Petal.Length", "Petal.Width")
nbins <- c(2,2)

# -------
## Make ensemble based on combination/binning type
weightedEnsemble <- buildWeightedEnsemble(train, modelList, weightType, comb_rule, bin_type, bin_features, nbins)

# -------
## Test it
#
predictEnsemble(weightedEnsemble, test[,-5])

eval_ensemble(weightedEnsemble, test)


# test_data <- test
# train_data <- train
# ensemble <- weightedEnsemble
# train_data_preds <- ensemble$trainPreds

#--------------------------------------------------------------------------
## Phonome Example
##
## Load train and test data
##
set.seed(12345)
train_index <- sample(1:5000, 2500)
train <- binClassifieR::phoneme[train_index,]
test <- binClassifieR::phoneme[-train_index,]


##
## Build models to stack
##
# naiveBayes <- RWeka::make_Weka_classifier("weka.classifiers.bayes.NaiveBayes")
# model1 <- naiveBayes(true_class ~., train)
# 
# randomForest <- RWeka::make_Weka_classifier("weka.classifiers.trees.RandomForest")
# model2 <- randomForest(true_class ~., train)
# 
# logistic <- RWeka::make_Weka_classifier("weka.classifiers.functions.Logistic")
# model3 <- logistic(true_class ~., train)
# 
# knn <- RWeka::make_Weka_classifier("weka.classifiers.lazy.IBk")
# model4 <- logistic(true_class ~., train)
# 
# modelList <- list(model1, model2, model3, model4)
# 
# modelList[[1]]

## Specify member classifiers with function
model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.meta.Bagging")

modelList <- make_model_list(model_types, test)

## train
##
weightType <- "bin weighted"
# weightType <- "weighted"
# weightType <- "bin dictator"
comb_rule <- "majority vote"
# comb_rule <- "average posterior"
# bin_type <- "quantile"
# bin_type <- "iterative quantile"
bin_type <- "standard"
bin_features <- c("Aa", "Iy")
nbins <- c(2,2)
weightedEnsemble <- buildWeightedEnsemble(train, modelList, weightType, comb_rule, bin_type, bin_features, nbins)

##
## test
# ##
# head(test[,-6])
predictEnsemble(weightedEnsemble, test[,-6])
# 


eval_ensemble(weightedEnsemble, test)


# s1 <- paste("Ensemble accuracy: ", RA(table(test$true_class, predictEnsemble(weightedEnsemble, test[,-6]))), sep = "")
# s2 <- paste("Model 1 accuracy: ", RA(table(test$true_class, predict(model1, test[,-6]))), sep = "")
# s3 <- paste("Model 2 accuracy: ", RA(table(test$true_class, predict(model2, test[,-6]))), sep = "")
# s4 <- paste("Model 3 accuracy: ", RA(table(test$true_class, predict(model3, test[,-6]))), sep = "")
# 
# s1
# s2
# s3
# s4



