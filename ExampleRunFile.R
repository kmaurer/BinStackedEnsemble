setwd("~/GitHub/BinStackedEnsemble/R")
source("binningFunctionsUpdated.R")
source("predictFunctionsUpdated.R")
source("trainFunctionsUpdated.R")
source("weightingFunctionsUpdated.R")
source("SimulationStudyFunctions.R")


# source experimental Iterative Quantile binning functions
source("C:\\Users\\maurerkt\\Documents\\GitHub\\IQbinR\\IterativeQuantileBinningSupportFunctions.R")
source("C:\\Users\\maurerkt\\Documents\\GitHub\\IQbinR\\IterativeQuantileBinning.R")

options(java.parameters = "-Xmx2g")


### Using Testing Functions individually
# Separate into training/test
names(iris)[5] <- "true_class"
train_index <- c(sample(1:50, 30),sample(51:100, 30),sample(101:150, 30))
train <- iris[train_index, ]
test <- iris[-train_index, ]
# Specify model list
model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO")
modelList <- make_model_list(model_types, train)

# collect results of all unbinned ensemble options
unbinned_results <- unbinned_testing(train, test, modelList)
unbinned_results

# collect results of all rectangular binned ensemble options
# need features and nbins

(nbins_list <- make_nbins_list_pairs(2:3))
(bin_features_list <- make_bin_feature_list_pairs(c("Petal.Length","Petal.Width","Sepal.Length"), ordered=FALSE))
rect_binned_results <- binned_testing(train, test, modelList, bin_features_list, nbins_list)
rect_binned_results


# collect results of all unbinned ensemble options
# bin feature order matters for IQ binning (make bin_feature_list with both orders)

(nbins_list <- make_nbins_list_pairs(2:3))
(bin_features_list <- make_bin_feature_list_pairs(c("Petal.Length","Petal.Width","Sepal.Length"), ordered=TRUE))
iq_binned_results <- iq_binned_testing(train, test, modelList, bin_features_list, nbins_list)
iq_binned_results



### Using Testing Functions in Combined
names(iris)[5] <- "true_class"
train_index <- c(sample(1:50, 30),sample(51:100, 30),sample(101:150, 30))
# train_data <- iris[train_index, ]
# test_data <- iris[-train_index, ]
# Specify model list
model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO")
bin_features_all <- c("Petal.Length","Petal.Width")
nbins_all <- 2:3

# results_all <- testing_all_ensembles(train_data,test_data,model_types,bin_features_all,nbins_all)
cv_results_all_iris <- cv_testing_all_ensembles(data=iris,model_types=model_types,bin_features_all=bin_features_all,nbins_all=nbins_all,equal_bins=TRUE,cv_K=2)



# set.seed(12345)
# # Specify model list
# phonome <- binClassifieR::phoneme
# model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
#                  "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO","weka.classifiers.functions.Logistic")
# bin_features_all <- c("Aa","Ao","Iy")
# nbins_all <- 2:3
# # results_all <- testing_all_ensembles(train_data,test_data,model_types,bin_features_all,nbins_all)
# cv_results_all_iris <- cv_testing_all_ensembles(data=phonome,model_types=model_types,bin_features_all=bin_features_all,nbins_all=nbins_all,cv_K=5)
# cv_results_all_iris
# save(cv_results_all_iris,file="cv_results_all_phonome.Rdata")


load(file="cv_results_all_phonome.Rdata")
library(tidyverse)
cv_results_all_iris$unbinned_results

cv_results_all_iris$rect_binned_results %>%
  group_by(weight_type, comb_type,bin_type) %>%
  summarize(tuned_accuracy = max(accuracy),
            nbins_name=nbins_name[which.max(accuracy)],
            bin_pair_name=bin_pair_name[which.max(accuracy)])

cv_results_all_iris$iq_binned_results %>%
  group_by(weight_type, comb_type,bin_type) %>%
  summarize(tuned_accuracy = max(accuracy),
            nbins_name=nbins_name[which.max(accuracy)],
            bin_pair_name=bin_pair_name[which.max(accuracy)])

#--------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
load("C:\\Users\\maurerkt\\Documents\\onePercentSample.Rdata")
head(onePercentSample)
str(onePercentSample)

set.seed(12345)
taxi <- onePercentSample %>%
  select(payment_type,pickup_datetime,passenger_count,trip_distance,pickup_longitude,pickup_latitude,fare_amount,tip_amount) %>%
  filter(payment_type %in% c("credit","cash")) %>%
  na.omit() %>% 
  mutate(hour = hour(pickup_datetime),
         wday = wday(pickup_datetime),
         payment_type = factor(payment_type)) %>%
  select(-pickup_datetime) %>%
  sample_n(20000)
names(taxi)[1] <- "true_class"
head(taxi)

remove(onePercentSample)



model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO","weka.classifiers.functions.Logistic")
bin_features_all <- c("pickup_longitude","pickup_latitude")
nbins_all <- 2:6
# results_all <- testing_all_ensembles(train_data,test_data,model_types,bin_features_all,nbins_all)
cv_results_all_taxi <- cv_testing_all_ensembles(data=taxi,model_types=model_types,bin_features_all=bin_features_all,nbins_all=nbins_all,equal_bins=TRUE, cv_K=10)
cv_results_all_taxi
# save(cv_results_all_taxi,file="cv_results_all_taxi.Rdata")

cv_results_all_taxi$unbinned_results

cv_results_all_taxi$rect_binned_results %>%
  group_by(weight_type, comb_type,bin_type) %>%
  summarize(tuned_accuracy = max(accuracy),
            nbins_name=nbins_name[which.max(accuracy)],
            bin_pair_name=bin_pair_name[which.max(accuracy)])

cv_results_all_taxi$iq_binned_results %>%
  group_by(weight_type, comb_type,bin_type) %>%
  summarize(tuned_accuracy = max(accuracy),
            nbins_name=nbins_name[which.max(accuracy)],
            bin_pair_name=bin_pair_name[which.max(accuracy)])
#---------------------------------------------------------------------------

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
bin_features <- c("Petal.Length", "Petal.Width","Sepal.Width")
nbins <- c(1,2,3)

# -------
## Make ensemble based on combination/binning type

train_preds <- make_train_preds(train,modelList)
weightedEnsemble <- make_ensemble(train_preds, modelList, weightType, comb_rule, bin_type, bin_features, nbins)

# -------
## Test it
#
predictEnsemble(weightedEnsemble, test)

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


## Specify member classifiers with function

model_types <- c("weka.classifiers.trees.RandomForest","weka.classifiers.functions.SMO","weka.classifiers.functions.Logistic")
bin_features_all <- c("pickup_longitude","pickup_latitude")
nbins_all <- 2:3
modelList <- make_model_list(model_types, test)

unbinned_testing(train,test,modelList)

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
bin_features <- c("pickup_longitude","pickup_latitude")
nbins <- c(2,2)
weightedEnsemble <- make_ensemble(train, modelList, weightType, comb_rule, bin_type, bin_features, nbins)

##
## test
# ##
# head(test[,-6])
predictEnsemble(weightedEnsemble,test)
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

#--------------------------------------------------------------------------
## Phonome Example
##
## Load train and test data
##
set.seed(12345)
train_index <- sample(1:20000, 16000)
train <- taxi[train_index,]
test <- taxi[-train_index,]


## Specify member classifiers with function
model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO","weka.classifiers.functions.Logistic")

modelList <- make_model_list(model_types, train)
predict(modelList[[4]], test) == test$true_class

## train
##
train_preds <- make_train_preds(train,modelList)


# weightType <- "bin weighted"
# weightType <- "weighted"
weightType <- "bin dictator"
comb_rule <- "majority vote"
# comb_rule <- "average posterior"
# bin_type <- "quantile"
# bin_type <- "iterative quantile"
bin_type <- "standard"
bin_features <- c("pickup_longitude","pickup_latitude")
nbins <- c(2,2)
weightedEnsemble <- make_ensemble(train_preds, modelList, weightType, comb_rule, bin_type, bin_features, nbins)

##
## test
# ##
# head(test[,-6])
predictEnsemble(weightedEnsemble, test)
# 

eval_ensemble(weightedEnsemble, test)


#-----------------------------------------------------------------------------

load(file="../data/cover_type.Rdata")
set.seed(12345)
index <- sample(1:nrow(cover_type),100000)
train <- cover_type[index[1:80000],]
test <- cover_type[index[80001:100000],]

## Specify member classifiers with function
model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO","weka.classifiers.functions.Logistic")

modelList <- make_model_list(model_types, train)
sum(predict(modelList[[5]], test) == test$true_class)

bin_features_all <- c("elevation","hori_dist_road","hori_dist_fire","hori_dist_hydro")
nbins_all <- 2:4
# results_all <- testing_all_ensembles(train_data,test_data,model_types,bin_features_all,nbins_all)
set.seed(12345)
cover_samp <- dplyr::sample_n(cover_type,10000)
remove(cover_type)
cv_results_cover_type <- cv_testing_all_ensembles(data=cover_samp,model_types=model_types,bin_features_all=bin_features_all,nbins_all=nbins_all,equal_bins=TRUE, cv_K=10)
cv_results_cover_type

# save(cv_results_cover_type,file="cv_results_cover_type_10000.Rdata")

cv_results_cover_type$unbinned_results

cv_results_cover_type$rect_binned_results %>%
  group_by(weight_type, comb_type,bin_type) %>%
  summarize(tuned_accuracy = max(accuracy),
            nbins_name=nbins_name[which.max(accuracy)],
            bin_pair_name=bin_pair_name[which.max(accuracy)])

cv_results_cover_type$iq_binned_results %>%
  group_by(weight_type, comb_type,bin_type) %>%
  summarize(tuned_accuracy = max(accuracy),
            nbins_name=nbins_name[which.max(accuracy)],
            bin_pair_name=bin_pair_name[which.max(accuracy)])

