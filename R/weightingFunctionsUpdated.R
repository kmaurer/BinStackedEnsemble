#' Average posterior vote
#'
#' @description Create ensemble combination metric arrays with dimension \code{n X K X M}
#' with elements v_lim = obs i metric for class l from member m
#' Using average posterior rule\cr\cr
#'
#'\code{n} is the number of observations\cr
#'\code{K} is the number of classes\cr
#'\code{M} is the number of models\cr
#'
#' I DO NOT YET UNDERSTAND THIS ONE
#'
#' @param test_post_probs Posterior probabilities from model predictions on test data
#' @return \code{model_metric}
#' @examples
#'
#' @export
make_model_metric_array_avgpost <- function(test_post_probs){
  n = nrow(test_post_probs[[1]])                                # number of observations
  K = ncol(test_post_probs[[1]])                                # number of classes
  M = length(test_post_probs)                                   # number of models
  model_metric <- array(NA,c(n,K,M))
  for(m in 1:M){
    model_metric[,,m] <- test_post_probs[[m]]
  }
  dimnames(model_metric) <- list(dimnames(test_post_probs[[1]])[[1]],dimnames(test_post_probs[[1]])[[2]],1:M)
  return(model_metric)
}

#' Majority vote
#'
#' @description Create ensemble combination metric arrays with dimension n X K X M
#' with elements v_lim = obs i metric for class l from member m
#' Using majority rule
#'
#' I DO NOT YET UNDERSTAND THIS ONE
#'
#' @param test_post_probs Posterior probabilities from model predictions on test data
#' @return \code{model_metric}
#' @examples
#'
#' @export
make_model_metric_array_majvote <- function(test_preds){
  n = nrow(test_preds)
  K = length(levels(test_preds[,1]))
  M = ncol(test_preds)
  model_metric <- array(NA,c(n,K,M))
  for(m in 1:M){
    for(l in 1:K){
      for(i in 1:n){
        model_metric[i,l,m] <- as.numeric(test_preds[i,m]==levels(test_preds[,1])[l])
      }
    }
  }
  dimnames(model_metric) <- list(row.names(test_preds),levels(test_preds[,1]),1:M)
  return(model_metric)
}

#' Model metric array for combination rules
#'
#' @description Function for making model metric array for combination rules\cr
#' I DO NOT YET UNDERSTAND THIS ONE \cr
#' NEEDS TO BE GENERALIZED
#'
#' @param combination_rule "majority vote" or "average posterior"
#' @param model_storage_list A list holding models from RWeka
#' @param test_data A data frame holding data on which to test
#' @param true_classes An array holding the order of the true labels CANDIDATE FOR REPLACEMENT

#' @return \code{model_metric}
#' @examples
#'
#' @export
make_model_metric_array <- function(combination_rule, model_storage_list, test_data, true_classes){

  model_metric = NULL
  if(combination_rule == "majority vote"){
    test_preds <- as.data.frame(matrix(0, ncol = length(model_storage_list), nrow = dim(test_data)[1]))
    for(i in 1:length(model_storage_list)){
      test_preds[,i] <- factor(predict(model_storage_list[[i]], type = "class", newdata = test_data), levels = true_classes)
    }
    model_metric <- make_model_metric_array_majvote(test_preds)
  }
  if(combination_rule == "average posterior"){
    test_preds <- list()
    for(i in 1:length(model_storage_list)){
      test_preds[[i]] <- predict(model_storage_list[[i]], type = "probability", newdata = test_data)
    }
    model_metric <- make_model_metric_array_avgpost(test_preds)
  }
  return(model_metric)
}

#' Model weights for "weight_type == "weighted"
#'
#' @description Calculate the model weights when "weight_type" == "weighted"
#'
#' @param train_data Training data with predicted class columns from each model \code{1,...,M}
#' @param n The number of instances in the test data
#' @export
weighted <- function(train_data, M, n){
  K <- length(levels(train_data$true_class))
  model_accuracies <- array(sapply(paste("preds",1:M,sep=""), function(x){
    sum(train_data$true_class==train_data[,x])
  }), dim=c(M,1))
  # ADD TRAINING ACCURACY BIAS CORRECTIONS HERE IN FUTURE?
  model_weights <- array(NA,c(n,K,M))
  for(k in 1:K){
    for(m in 1:M){
      model_weights[,k,m] <- model_accuracies[m]
    }
  }
  return(model_weights)
}

#' Model weights for "weight_type == "bin weighted"
#'
#' @description Calculate the model weights when "weight_type" == "bin weighted"
#' # TODO this needs to be cleaned up
#' @param train_data_preds Training data with predicted class columns from each model \code{1,...,M}
#' @param n The number of instances in the test data
#' @export
bin_weighted <- function(bin_features, bin_type, nbins, train_data_preds, test_data, M, K){

  n <- nrow(test_data)
  featurePairs <- make_feature_pair_df(bin_features, bin_type, nbins)
  featPairInfo <- featurePairs[1,]                                        # TODO this is redundant, but only because I've limited the number of feature pairs to one

  # merge data based on bin centers (round to avoid rounding mismatches)
  bin_train_dat <- round_df(add_bin_features(train_data_preds, bin_features,bin_type=bin_type,nbins=nbins),10)
  train_index <-round_df(make_train_bins_index(train_data_preds, featurePairs, bin_type = bin_type, nbins = nbins),10)
  bin_train_dat <- merge(bin_train_dat,train_index, all.x=TRUE)
  B = length(levels(bin_train_dat$index))
  # calculate bin accuracies for each model using training data
  # ADD TRAINING ACCURACY BIAS CORRECTIONS HERE IN FUTURE?
  bin_accuracy_array <- array(NA,c(M,B), dimnames=list(1:M,levels(bin_train_dat$index)))
  for(i in 1:M){
    for(j in levels(bin_train_dat$index)){
      inBin <- which(bin_train_dat$index==j)
      bin_accuracy_array[i,as.numeric(as.character(j))] <- sum(diag(table(train_data_preds$true_class[inBin],train_data_preds[,paste("preds",i,sep="")][inBin])))/length(inBin)
    }
    bin_accuracy_array[i,][is.na(bin_accuracy_array[i,])] <- 0
  }
  # set weights for test data observations based on bin they belong to
  model_weights <- array(NA,c(n,K,M))
  test_bins <- as.data.frame(bin_test_by_train(train_data_preds,test_data,bin_features,bin_type, nbins))
  test_bins <- round(test_bins[,as.character(featPairInfo[1,3:4])],10)
  # bin_test_dat <- dplyr::left_join(test_bins,train_index)
  bin_test_dat <- merge(test_bins,train_index, all.x=TRUE, by=names(train_index)[1:length(bin_features)])
  for(b in as.numeric(as.character(unique(bin_test_dat$index)))){
    # for(k in 1:K){
    for(m in 1:M){
      binSet <- which(as.numeric(as.character(bin_test_dat$index))==b)
      model_weights[binSet,1:K,m] <- bin_accuracy_array[m,b]
    }
  }
  return(model_weights)
}

#----------------------------------------------------------------------------------------------------------

#' Model weights for "weight_type == "bin dictators"
#'
#' @description Calculate the model weights when "weight_type == "bin dictators". Weights will be given only to models that are best in each bin (allows for ties)
#' # TODO this needs to be cleaned up
#' @param train_data_preds Training data with predicted class columns from each model \code{1,...,M}
#' @param n The number of instances in the test data
#' @export
bin_dictator_weighted <- function(bin_features, bin_type, nbins, train_data_preds, test_data, M, K){
  
  n <- nrow(test_data)
  featurePairs <- make_feature_pair_df(bin_features, bin_type, nbins)
  featPairInfo <- featurePairs[1,]                                        # TODO this is redundant, but only because I've limited the number of feature pairs to one
  
  # merge data based on bin centers (round to avoid rounding mismatches)
  bin_train_dat <- round_df(add_bin_features(train_data_preds, bin_features,bin_type=bin_type,nbins=nbins),10)
  train_index <-round_df(make_train_bins_index(train_data_preds, featurePairs, bin_type = bin_type, nbins = nbins),10)
  bin_train_dat <- merge(bin_train_dat,train_index, all.x=TRUE)
  B = length(levels(bin_train_dat$index))
  # calculate bin accuracies for each model using training data
  # ADD TRAINING ACCURACY BIAS CORRECTIONS HERE IN FUTURE? YES! These need to be cross validated accuracy values
  bin_accuracy_array <- array(NA,c(M,B), dimnames=list(1:M,levels(bin_train_dat$index)))
  for(m in 1:M){
    for(j in levels(bin_train_dat$index)){
      inBin <- which(bin_train_dat$index==j)
      bin_accuracy_array[m,as.numeric(as.character(j))] <- sum(diag(table(train_data_preds$true_class[inBin],train_data_preds[,paste("preds",m,sep="")][inBin])))/length(inBin)
    }
    bin_accuracy_array[m,][is.na(bin_accuracy_array[m,])] <- 0
  }
  # set weights for test data observations based on bin they belong to 
  model_weights <- array(NA,c(n,K,M))
  test_bins <- as.data.frame(bin_test_by_train(train_data_preds,test_data,bin_features,bin_type, nbins))
  test_bins <- round(test_bins[,as.character(featPairInfo[1,3:4])],10)
  bin_test_dat <- merge(test_bins,train_index, all.x=TRUE, by=names(train_index)[1:length(bin_features)])
  for(b in as.numeric(as.character(unique(bin_test_dat$index)))){
    # indeces for observations in bin b
    binSet <- which(as.numeric(as.character(bin_test_dat$index))==b)
    # weight of 1 ONLY given models tied for best in bin (the bins "tiny dictator")
    model_weights[binSet,1:K,] <- as.numeric(bin_accuracy_array[,b] == max(bin_accuracy_array[,b]))
  }
  return(model_weights)
}












