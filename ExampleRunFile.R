setwd("~/GitHub/BinStackedEnsemble/R")
source("binningFunctionsUpdated.R")
source("predictFunctionsUpdated.R")
source("trainFunctionsUpdated.R")
source("weightingFunctionsUpdated.R")


##
## Function to calculate accuracy
##
RA <- function(confusion_matrix){
  row_dim<-dim(confusion_matrix)[1]
  s1<-1
  diag_sum<-0
  accuracy<-0
  while(s1<=row_dim)
  {
    s2<-1
    while(s2<=row_dim)
    {
      if(s1==s2) {
        diag_sum<-diag_sum+confusion_matrix[s1,s2]
      }
      s2<-s2+1 }
    s1<-s1+1 }
  accuracy<-diag_sum/sum(confusion_matrix)
  return(accuracy)
}


#--------------------------------------------------------------------------
# Iris example
##
names(iris)[5] <- "true_class"
train_index <- c(sample(1:50, 30),sample(51:100, 30),sample(101:150, 30))
train <- iris[train_index, ]
test <- iris[-train_index, ]


# ##
# ## Build models to stack
# ##
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


model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO")
# Function for building model list given Weka names
make_model_list <- function(model_types, data, ...){
  model_list <- list(NULL)
  for(m in 1:length(model_types)){
    mod_m_fct <- RWeka::make_Weka_classifier(model_types[m])
    model_list[[m]] <- mod_m_fct(true_class ~., train)
  }
  names(model_list) <- model_types
  return(model_list)
}
modelList <- make_model_list(model_types, test)
modelList[[1]]

predict(modelList[[4]], test)
##
## train
##
weightType <- "bin weighted"
# weightType <- "bin dictator"
# comb_rule <- "majority vote"
comb_rule <- "average posterior"
bin_type <- "quantile"
# bin_type <- "standard"
bin_features <- c("Petal.Length", "Petal.Width")
nbins <- 3
weightedEnsemble <- buildWeightedEnsemble(train, modelList, weightType, comb_rule, bin_type, bin_features, nbins)


##
## test
##
head(test[,-5])
predictEnsemble(weightedEnsemble, test[,-5])


# Function to evaluated the ensemble test accuracy against member accuracies
eval_ensemble <- function(ensemble, test_data){
  acc_df <- data.frame(model = c("Ensemble", names(ensemble$model_storage_list)),
                       accuracy = NA)
  acc_df$accuracy[1] <- RA(table(test_data$true_class, predictEnsemble(ensemble, test_data[,-which(names(test_data)=="true_class")])))
  for(m in 1:length(ensemble$model_storage_list)){
    acc_df$accuracy[m+1] <- RA(table(test_data$true_class, predict(ensemble$model_storage_list[[m]], test_data[,-which(names(test_data)=="true_class")])))
  }
  return(acc_df)
}
eval_ensemble(weightedEnsemble, test)


# s1 <- paste("Ensemble accuracy: ", ), sep = "")
# s2 <- paste("Model 1 accuracy: ", RA(table(test$true_class, predict(model1, test[,-6]))), sep = "")
# s3 <- paste("Model 2 accuracy: ", RA(table(test$true_class, predict(model2, test[,-6]))), sep = "")
# s4 <- paste("Model 3 accuracy: ", RA(table(test$true_class, predict(model3, test[,-6]))), sep = "")
# s5 <- paste("Model 4 accuracy: ", RA(table(test$true_class, predict(model4, test[,-6]))), sep = "")
# 
# s1
# s2
# s3
# s4
# s5


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
naiveBayes <- RWeka::make_Weka_classifier("weka.classifiers.bayes.NaiveBayes")
model1 <- naiveBayes(true_class ~., train)

randomForest <- RWeka::make_Weka_classifier("weka.classifiers.trees.RandomForest")
model2 <- randomForest(true_class ~., train)

logistic <- RWeka::make_Weka_classifier("weka.classifiers.functions.Logistic")
model3 <- logistic(true_class ~., train)

knn <- RWeka::make_Weka_classifier("weka.classifiers.lazy.IBk")
model4 <- logistic(true_class ~., train)

modelList <- list(model1, model2, model3)

modelList[[1]]



## train
##
weightType <- "bin weighted"
# weightType <- "weighted"
# weightType <- "bin dictator"
# comb_rule <- "majority vote"
comb_rule <- "average posterior"
# bin_type <- "quantile"
bin_type <- "standard"
bin_features <- c("Aa", "Ao")
nbins <- 3
weightedEnsemble <- buildWeightedEnsemble(train, modelList, weightType, comb_rule, bin_type, bin_features, nbins)

##
## test
##
head(test[,-6])
predictEnsemble(weightedEnsemble, test[,-6])

s1 <- paste("Ensemble accuracy: ", RA(table(test$true_class, predictEnsemble(weightedEnsemble, test[,-6]))), sep = "")
s2 <- paste("Model 1 accuracy: ", RA(table(test$true_class, predict(model1, test[,-6]))), sep = "")
s3 <- paste("Model 2 accuracy: ", RA(table(test$true_class, predict(model2, test[,-6]))), sep = "")
s4 <- paste("Model 3 accuracy: ", RA(table(test$true_class, predict(model3, test[,-6]))), sep = "")

s1
s2
s3
s4



