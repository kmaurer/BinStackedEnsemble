# function to build models WITHIN each bin
make_bin_fitted_model_list <- function(model_types,bin_train,train_data){
  bin_fitted_model_list <- list(NULL)
  for(b in unique(bin_train$bin_data$bin_index)){
    bin_fitted_model_list[[b]] <- list(NULL)
    bin_train_b <- train_data[bin_train$bin_data$bin_index==b, ]
    for(m in 1:length(model_types)){
      mod_m_fct <- RWeka::make_Weka_classifier(model_types[m])
      bin_fitted_model_list[[b]][[m]] <- mod_m_fct(true_class ~., data=bin_train_b)
    }
    names(bin_fitted_model_list[[b]]) <- model_types
  }
  return(bin_fitted_model_list)
}

make_bin_fitted_weights <- function(bin_fitted_model_list, bin_train,train_data, bin_test,test_data,true_classes){
  M=length(bin_fitted_model_list[[1]])
  B=nrow(bin_train$bin_def$bin_centers)
  # true_classes <- levels(train_data[,"true_class"])
  preds <- as.data.frame(matrix(rep(NA,nrow(train_data)*M),ncol=M))
  names(preds) <- paste0("preds",1:M)
  for(b in 1:B){
      in_bin_b <- which(bin_train$bin_data$bin_index==b)
      bin_train_b <- train_data[in_bin_b, ]
      preds[in_bin_b, ] <- make_preds(data=bin_train_b,model_list = bin_fitted_model_list[[b]],true_classes=true_classes)
  }
  preds <- sapply(paste0("preds",1:M), function(x) factor(preds[,x], labels=true_classes[unique(preds[,x])]))
  train_data_preds <- cbind(train_data, preds)  
    
  model_accuracies <- sapply(paste("preds",1:M,sep=""), function(x){
    sum(as.character(train_data_preds$true_class)==as.character(train_data_preds[,x]))/nrow(train_data_preds)
  })
  bin_accuracy_array <- matrix(rep(model_accuracies,B),c(M,B), dimnames=list(1:M,1:B))
  for(m in 1:M){
    for(b in unique(bin_train$bin_data$bin_index)){
      inBin <- which(bin_train$bin_data$bin_index==b)
      bin_accuracy_array[m,as.numeric(as.character(b))] <- sum(as.character(train_data_preds$true_class[inBin])==as.character(train_data_preds[,paste("preds",m,sep="")][inBin]))/length(inBin)
    }
  }
  
  ## set weights for test data observations based on the training accuracies of the bin they belong to
  n=nrow(test_data)
  model_weights <- array(NA,c(n,M))
  for(b in unique(bin_test$bin_indeces)){
    binSet <- bin_test$bin_indeces==b
    model_weights[binSet,] <- bin_accuracy_array[,b]
  }
  return(model_weights)
}

  
make_bin_fitted_model_metric_array <- function(combination_rule, bin_fitted_model_list, test_data, bin_test, true_classes){
  model_metric = NULL
  if(combination_rule == "majority vote"){
    test_preds <- as.data.frame(matrix(0, ncol = length(bin_fitted_model_list[[1]]), nrow = dim(test_data)[1]))
    for(b in unique(bin_test$bin_indeces)){
      binSet <- bin_test$bin_indeces==b
      for(m in 1:length(bin_fitted_model_list[[1]])){
        test_preds[binSet,m] <- predict(bin_fitted_model_list[[b]][[m]], type = "class", newdata = test_data[binSet,])
      }
    }
    for(j in 1:ncol(test_preds)){
      test_preds[,j] <- factor(test_preds[,j], labels=true_classes[unique(test_preds[,j])])
    }
    model_metric <- make_model_metric_array_majvote(test_preds)
  }
  if(combination_rule == "average posterior"){
    test_preds <- list()
    for(m in 1:length(bin_fitted_model_list[[1]])){
      test_preds[[m]] <- matrix(0, ncol = length(true_classes), nrow = dim(test_data)[1])
      for(b in unique(bin_test$bin_indeces)){
        binSet <- bin_test$bin_indeces==b
        test_preds[[m]][binSet,] <- predict(bin_fitted_model_list[[b]][[m]], type = "probability", newdata = test_data[binSet,])
      }
    }
    model_metric <- make_model_metric_array_avgpost(test_preds)
  }
  return(model_metric)
}  



predict_bin_fitted_ensemble <- function(model_types, bin_features, bin_type, nbins, train_data, test_data, comb_rule, weightType, true_classes){
  n <- nrow(test_data)    # how many predictions to make
  # true_classes <- levels(train_data$true_class)
  K <- length(true_classes)        # the number of classes
  M <- length(model_types)                            # the number of models
  
  ## Start with creating bin definitions based on "training data" then bin "test data" with that definition
  if(bin_type %in% c("standard","quantile")){
    bin_train <- bin_nd(data=train_data, bin_features=bin_features, nbins=nbins, bin_type=bin_type, output="both")
    bin_test <- bin_nd_by_def(test_data, bin_nd_def=bin_train$bin_def)
  } else if(bin_type=="iterative quantile"){
    bin_train <- iterative_quant_bin(data=train_data, bin_cols=bin_features, nbins=nbins, output="both", jit=rep(.001,length(nbins)))
    bin_test <- bin_by_IQdef(iq_def=bin_train$bin_def, new_data=test_data, output="data", strict=FALSE)
  } 
  bin_fitted_model_list <- make_bin_fitted_model_list(model_types,bin_train,train_data)
  
  # Make model weights for test data based on CV training accuracy
  modelWeights <- make_bin_fitted_weights(bin_fitted_model_list, bin_train,train_data, bin_test,test_data, true_classes)
  if(weightType == "bin dictator") modelWeights <- bin_dictator_weighted(modelWeights)
  
  ##
  ## Make predictions
  ##
  model_votes <- make_bin_fitted_model_metric_array(comb_rule, bin_fitted_model_list, test_data, bin_test, true_classes)
  
  pred <- rep(NA,nrow(test_data))
  for(i in 1:nrow(test_data)){
    combination_class_results <- rep(NA,K)
    for(k in 1:K){
      combination_class_results[k] <- modelWeights[i,] %*% model_votes[i,k,]
    }
    # Assign predicted classes based on maximized combination rule
    pred[i] <- true_classes[which.max(combination_class_results)]
  }
  pred <- factor(pred,levels=true_classes)
  names(pred) <- row.names(test_data)
  return(pred)
}







# Iris example
#
names(iris)[5] <- "true_class"
train_index <- c(sample(1:50, 30),sample(51:100, 30),sample(101:150, 30))
train_data <- iris[train_index, ]
test_data <- iris[-train_index, ]
# -------
## Specify member classifiers with function
model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO")
# -------
## Specify combination rules and binning types
# weightType <- "bin weighted"
weightType <- "bin dictator"
comb_rule <- "majority vote"
# comb_rule <- "average posterior"
# bin_type <- "quantile"
# bin_type <- "standard"
bin_type <- "iterative quantile"
bin_features <- c("Petal.Length", "Petal.Width","Sepal.Width")
nbins <- c(2,2,3)
true_classes <- levels(iris$true_class)

predict_bin_fitted_ensemble(model_types=model_types, bin_features=bin_features, bin_type=bin_type, nbins=nbins,
                            train_data=train_data, test_data=test_data, comb_rule=comb_rule, weightType=weightType,
                            true_classes=true_classes)

(nbins_list <- make_nbins_list_pairs(2:3))
(bin_features_list <- make_bin_feature_list_pairs(c("Petal.Length","Petal.Width"), ordered=FALSE))





iq_bin_fitted_testing <- function(train_data, test_data, model_types, bin_features_list, nbins_list,true_classes){
  # Make data.frame with all "treatment" combinations
  results <- expand.grid(c("bin weighted","bin dictator"),c("majority vote","average posterior"),"iterative quantile",
                         1:length(nbins_list),1:length(bin_features_list), stringsAsFactors = FALSE)
  names(results) <- c("weight_type","comb_type","bin_type","nbins","bin_features")
  nbins_names <- sapply(nbins_list,function(x) paste(as.character(x), collapse=" X ") )
  bin_features_names <- sapply(bin_features_list,function(x) paste(as.character(x), collapse=" X ") )
  results$bin_pair_name <- bin_features_names[results$bin_features]
  results$nbins_name <-   nbins_names[results$nbins]
  results$accuracy <- NA
  # Loop over treatments and save results
  for(i in 1:nrow(results)){
    preds <- predict_bin_fitted_ensemble(model_types=model_types, bin_features = bin_features_list[[results$bin_features[i]]],
                                         bin_type=results$bin_type[i], nbins=nbins_list[[results$nbins[i]]],
                                         train_data=train_data, test_data=test_data, comb_rule = results$comb_type[i],
                                         weightType=results$weight_type[i],
                                         true_classes=true_classes)
    results$accuracy[i] <- sum(preds == test_data$true_class)/nrow(test_data)
  }
  return(results)
}
# iq_bin_fitted_testing(train_data, test_data, model_types, bin_features_list, nbins_list)


testing_all_bin_fitted_ensembles <- function(train_data,test_data,model_types,bin_features_all,nbins_all,equal_bins=FALSE,true_classes){
  model_list <- make_model_list(model_types, train_data)
  # collect results of all unbinned ensemble options
  unbinned_results <- unbinned_testing(train_data, test_data, model_list,true_classes)
  # collect results of all iq bin fitted ensemble options
  nbins_list <- make_nbins_list_pairs(nbins_all,equal_bins)
  bin_features_list <- make_bin_feature_list_pairs(bin_features_all, ordered=FALSE)
  iq_bin_fitted_results <- iq_bin_fitted_testing(train_data, test_data, model_types, bin_features_list, nbins_list,true_classes)
  
  return(list(unbinned_results=unbinned_results,iq_bin_fitted_results=iq_bin_fitted_results))
}
# testing_all_bin_fitted_ensembles(train_data, test_data,model_types, bin_features_all=c("Petal.Length","Petal.Width"),nbins_all=2,equal_bins=TRUE, true_classes=true_classes)


cv_testing_all_bin_fitted_ensembles <- function(data,model_types,bin_features_all,nbins_all,equal_bins=FALSE, cv_K=10,true_classes){
  cv_index <- cv_cohorts(nrow(data),cv_K)
  results_list <- list(NULL)
  for(fold in 1:cv_K){
    train_data <- data[cv_index!=fold, ]
    test_data <- data[cv_index==fold, ]
    results_list[[fold]] <- testing_all_bin_fitted_ensembles(train_data,test_data,model_types,bin_features_all,nbins_all,equal_bins,true_classes)
  }
  results_list_all <- results_list[[1]]
  cv_weights <- sapply(1:cv_K, function(x) sum(cv_index==x))
  results_list_all[[1]]$accuracy <- sapply(1:nrow(results_list[[1]][[1]]), function(i) weighted.mean(sapply(1:cv_K, function(fold) results_list[[fold]][[1]]$accuracy)[i,],w=cv_weights))
  results_list_all[[2]]$accuracy <- sapply(1:nrow(results_list[[1]][[2]]), function(i) weighted.mean(sapply(1:cv_K, function(fold) results_list[[fold]][[2]]$accuracy)[i,],w=cv_weights))
  return(results_list_all)
}
# cv_testing_all_bin_fitted_ensembles(iris,model_types, bin_features_all=c("Petal.Length","Petal.Width"),nbins_all=2:3,equal_bins=TRUE, cv_K=2, true_classes=true_classes)

#--------------------------------------------------------------------------------------------------------------


## Load train and test data
##
all_dat <- binClassifieR::phoneme
set.seed(12345)
train_index <- sample(1:5000, 4500)
train <- binClassifieR::phoneme[train_index,]
test <- binClassifieR::phoneme[-train_index,]


## Specify member classifiers with function

model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.functions.SMO","weka.classifiers.functions.Logistic")
bin_features_all <- c("Aa","Ao","Iy")
nbins_all <- 2:3

test_bin_fit_phonome <- testing_all_bin_fitted_ensembles(train, test,model_types, bin_features_all,nbins_all,equal_bins=TRUE)
test_bin_fit_phonome

# cv_bin_fitted_phonome <- cv_testing_all_bin_fitted_ensembles(all_dat,model_types, bin_features_all,nbins_all=2:3,equal_bins=TRUE, cv_K=10)
# save(cv_bin_fitted_phonome, "cv_bin_fitted_phonome.Rdata")
load()
unbinned_testing(train,test,modelList)


(nbins_list <- make_nbins_list_pairs(2:4,equal_bins = TRUE))
(bin_features_list <- make_bin_feature_list_pairs(c("Aa","Ao","Dcl","Iy","Sh"), ordered=FALSE))

iq_bin_fitted_results <- iq_bin_fitted_testing(train, test, model_types, bin_features_list, nbins_list)
# save(iq_bin_fitted_results, file="iq_bin_fitted_results.Rdata")

library(tidyverse)

iq_bin_fitted_results %>% 
  group_by(weight_type, comb_type) %>%
  summarize(tuned_accuracy = max(accuracy),
            nbins_name=nbins_name[which.max(accuracy)],
            bin_pair_name=bin_pair_name[which.max(accuracy)])

#-----------------------------------------------------------------------------

load(file="../data/cover_type.Rdata")
## Specify member classifiers with function
model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO","weka.classifiers.functions.Logistic")
bin_features_all <- c("elevation","hori_dist_road","hori_dist_fire","hori_dist_hydro")
nbins_all <- 2:5
set.seed(12345)
cover_samp <- dplyr::sample_n(cover_type,10000)
remove(cover_type)

# cv_bin_fitted_covertype <- cv_testing_all_bin_fitted_ensembles(cover_samp,model_types, bin_features_all,nbins_all=2:5,equal_bins=TRUE, cv_K=10)
# save(cv_bin_fitted_covertype, "cv_bin_fitted_covertype.Rdata")


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
#------------------------------------------------------------------------------------
college <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/College.csv")
college<-college[, c(2, 6, 10, 12, 15)]
names(college)[1] <- "true_class"
college$true_class<-as.factor(college$true_class)

train_index<-sample(seq_len(nrow(college)), size=500)
train_data<-college[train_index, ]
test_data<-college[-train_index, ]

## Specify member classifiers with function
model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO")
modelList <- make_model_list(model_types, train_data)

unbinned_testing(train_data, test_data, modelList)

test_college_bin_fitted <- testing_all_bin_fitted_ensembles(train_data, test_data,model_types, 
                                                            bin_features_all=c("Top10perc","Outstate"),nbins_all=2,equal_bins=TRUE, true_classes=true_classes)
test_college_bin_fitted

test_college_bin_fitted_cv <-cv_testing_all_bin_fitted_ensembles(college ,model_types, bin_features_all=c("Top10perc","Outstate"),
                                                                 nbins_all=2,equal_bins=TRUE, cv_K=10, true_classes=true_classes)


############################################## abalone data
library(data.table)
abalone <- fread('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data')
abalone<-as.data.frame(abalone)
names(abalone)[1]<-"true_class"
abalone$true_class<-as.factor(abalone$true_class)
true_classes <- levels(abalone$true_class)

cv_test_abalone_bin_fitted <- cv_testing_all_bin_fitted_ensembles(abalone, model_types, 
                                                            bin_features_all=c("V2","V3"), nbins_all=2:3,
                                                            equal_bins=FALSE, cv_K=10, true_classes=true_classes)

cv_test_abalone <- cv_testing_all_ensembles(abalone, model_types, 
                                                       bin_features_all=c("V2","V3"), nbins_all=2:3,
                                                       equal_bins=FALSE, cv_K=10, true_classes=true_classes)