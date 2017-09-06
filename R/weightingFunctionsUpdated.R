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
  model_accuracies <- array(sapply(paste("preds",1:M,sep=""), function(x){
    sum(train_data$true_class==train_data[,x])
  }), dim=c(M,1))
  model_weights <- array(NA,c(n,M))
  for(m in 1:M){
    model_weights[,m] <- model_accuracies[m]
  }
  return(model_weights)
}

#' Model weights for "weight_type == "bin weighted" VERSION 2
#'
#' @description Calculate the model weights when "weight_type" == "bin weighted"
#' # TODO this needs to be cleaned up
#' @param bin_features Training data with predicted class columns from each model \code{1,...,M}
#' @param bin_type Type of binning {"standard","quantile","iterative quantile"}
#' @param nbins vector containing number of bins in each dimension
#' @param train_data_preds data frame containing training data and CV prediction columns
#' @param test_data data frame containing test data. Must have same column names as training data
#' @param M number of models in bin weighted ensemble
#' @param K number of true classes
#' @export
bin_weighted <- function(bin_features, bin_type, nbins, train_data_preds, test_data, M, K){
  ## Start with creating bin definitions based on "training data" then bin "test data" with that definition
  if(bin_type %in% c("standard","quantile")){
    bin_train <- bin_nd(data=train_data_preds, bin_features=bin_features, nbins=nbins, bin_type=bin_type, output="both")
    bin_test <- bin_nd_by_def(test_data, bin_nd_def=bin_train$bin_def)
  } else if(bin_type=="iterative quantile"){
    bin_train <- iterative_quant_bin(data=train_data_preds, bin_cols=bin_features, nbins=nbins, output="both", jit=rep(.001,length(nbins)))
    bin_test <- bin_by_iq_def(bin_def=bin_train$bin_def, new_data=test_data, output="data", strict=FALSE)
  } else {
    print("Please provide a supported bin_type")
    return(NULL)
  }
  ## Collect training accurcies of each bin using the cross validated predictions in train_data_preds
  # any region without existing data differs to overall model accuracies for weights
  B = nrow(bin_train$bin_def$bin_centers)
  model_accuracies <- sapply(paste("preds",1:M,sep=""), function(x){
    sum(train_data_preds$true_class==train_data_preds[,x])/nrow(train_data_preds)
  })
  bin_accuracy_array <- matrix(rep(model_accuracies,B),c(M,B), dimnames=list(1:M,1:B))
  for(m in 1:M){
    for(b in unique(bin_train$bin_data$bin_index)){
      inBin <- which(bin_train$bin_data$bin_index==b)
      bin_accuracy_array[m,as.numeric(as.character(b))] <- sum(train_data_preds$true_class[inBin]==train_data_preds[,paste("preds",m,sep="")][inBin])/length(inBin)
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

#----------------------------------------------------------------------------------------------------------

#' Establish bin dictator weights
#'
#' @description Take bin weights from bin_weights function and set all but best model weight in each bin to zero
#' 
#' @param bin_model_weights nXM matrix of weights from the bin_weighted function
#' @export
bin_dictator_weighted <- function(bin_model_weights){
  for(i in 1:nrow(bin_model_weights)){
    bin_model_weights[i,bin_model_weights[i,] < max(bin_model_weights[i,])] <- 0
  }
  return(bin_model_weights)
}












