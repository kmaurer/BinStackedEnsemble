#!# "help" documentation needs to be fixed/added for each function
#`` Exhaustively testing ensemble on new dataset
#'@description Predict using an ensemble classifier created from this package
#'
#'@param train_data Data to 
#'@param test_data Data to predict with the ensemble
#'@param model_list List of RWeka models fit to train_data
#'@param weight_type vector containing bin weight types from {"bin weighted","bin dictator"}
#'@param comb_rule vector containing combination rules from {"majority vote","average posterior"}
#'@param bin_type vector containing bin types from {"standard","quantile","iterative quantile"} 
#'@param bin_features vector containing names of columns to bin 
#'@param nbins vector containing number of bins in each dimension for bin_features (will use all combinations of these values)
#'
#'@return test accuracy rates for all weighting/voting/binning specifications requested
#'@export
#'
#'# -------
#'  train_index <- c(sample(1:50, 30),sample(51:100, 30),sample(101:150, 30))
unbinned_testing <- function(train_data, test_data, model_list){
  # Make data.frame with all "treatment" combinations
  results <- expand.grid(c("unweighted","weighted"),c("majority vote","average posterior"))
  names(results) <- c("weight_type","comb_type")
  results$accuracy <- NA
  train_preds <- make_train_preds(train_data,model_list)
  # Loop over treatments and save results
  for(i in 1:nrow(results)){
    weightedEnsemble <- make_ensemble(train_preds = train_preds, model_list = model_list,
                                              weightType = results$weight_type[i], comb_rule = results$comb_type[i])
    results$accuracy[i] <- sum(predictEnsemble(weightedEnsemble, test_data) == test_data$true_class)/nrow(test_data)
  }
  return(results)
}


binned_testing <- function(train_data, test_data, model_list, bin_features_list, nbins_list){
  # Make data.frame with all "treatment" combinations
  results <- expand.grid(c("bin weighted","bin dictator"),c("majority vote","average posterior"),c("standard","quantile"),
                         1:length(nbins_list),1:length(bin_features_list))
  names(results) <- c("weight_type","comb_type","bin_type","nbins","bin_features")
  nbins_names <- sapply(nbins_list,function(x) paste(as.character(x), collapse=" X ") )
  bin_features_names <- sapply(bin_features_list,function(x) paste(as.character(x), collapse=" X ") )
  results$bin_pair_name <- bin_features_names[results$bin_features]
  results$nbins_name <-   nbins_names[results$nbins]
  results$accuracy <- NA
  train_preds <- make_train_preds(train_data,model_list)
  # Loop over treatments and save results
  for(i in 1:nrow(results)){
    weightedEnsemble <- make_ensemble(train_preds = train_preds, model_list = model_list,
                                              weightType = results$weight_type[i], comb_rule = results$comb_type[i], 
                                              bin_type = results$bin_type[i], bin_features = bin_features_list[[results$bin_features[i]]],
                                              nbins = nbins_list[[results$nbins[i]]])
    results$accuracy[i] <- sum(predictEnsemble(weightedEnsemble, test_data) == test_data$true_class)/nrow(test_data)
  }
  return(results)
}


iq_binned_testing <- function(train_data, test_data, model_list, bin_features_list, nbins_list){
  # Make data.frame with all "treatment" combinations
  results <- expand.grid(c("bin weighted","bin dictator"),c("majority vote","average posterior"),c("iterative quantile"),
                         1:length(nbins_list),1:length(bin_features_list))
  names(results) <- c("weight_type","comb_type","bin_type","nbins","bin_features")
  nbins_names <- sapply(nbins_list,function(x) paste(as.character(x), collapse=" X ") )
  bin_features_names <- sapply(bin_features_list,function(x) paste(as.character(x), collapse=" X ") )
  results$bin_pair_name <- bin_features_names[results$bin_features]
  results$nbins_name <-   nbins_names[results$nbins]
  results$accuracy <- NA
  train_preds <- make_train_preds(train_data,model_list)
  # Loop over treatments and save results
  for(i in 1:nrow(results)){
    weightedEnsemble <- make_ensemble(train_preds = train_preds, model_list = model_list,
                                              weightType = results$weight_type[i], comb_rule = results$comb_type[i], 
                                              bin_type = results$bin_type[i], bin_features = bin_features_list[[results$bin_features[i]]],
                                              nbins = nbins_list[[results$nbins[i]]])
    results$accuracy[i] <- sum(predictEnsemble(weightedEnsemble, test_data) == test_data$true_class)/nrow(test_data)
  }
  return(results)
}



make_bin_feature_list_pairs <- function(bin_features_all,ordered=FALSE){
  bin_feature_list <- list(NULL)
  counter=0
  for(i in 1:(length(bin_features_all)-1)){
    for(j in (i+1):length(bin_features_all)){
      counter <- counter+1
      bin_feature_list[[counter]] <- bin_features_all[c(i,j)]
      if(ordered)
      counter <- counter+1
      bin_feature_list[[counter]] <- bin_features_all[c(j,i)]
    }
  }
  return(bin_feature_list)
}



make_nbins_list_pairs <- function(nbins_all){
  nbins_list <- list(NULL)
  counter=0
  for(i in 1:length(nbins_all)){
    for(j in i:length(nbins_all)){
      counter <- counter+1
      nbins_list[[counter]] <- nbins_all[c(i,j)]
      if(i != j){
        counter <- counter+1
        nbins_list[[counter]] <- nbins_all[c(j,i)]
      }
    }
  }
  return(nbins_list)
}




testing_all_ensembles <- function(train_data,test_data,model_types,bin_features_all,nbins_all){
  model_list <- make_model_list(model_types, train_data)
  # collect results of all unbinned ensemble options
  unbinned_results <- unbinned_testing(train_data, test_data, model_list)
  # collect results of all rectangular binned ensemble options
  nbins_list <- make_nbins_list_pairs(nbins_all)
  bin_features_list <- make_bin_feature_list_pairs(bin_features_all, ordered=FALSE)
  rect_binned_results <- binned_testing(train_data, test_data, model_list, bin_features_list, nbins_list)
  # collect results of all unbinned ensemble options
  nbins_list <- make_nbins_list_pairs(nbins_all)
  bin_features_list <- make_bin_feature_list_pairs(bin_features_all, ordered=TRUE)
  iq_binned_results <- iq_binned_testing(train_data, test_data, model_list, bin_features_list, nbins_list)

  return(list(unbinned_results=unbinned_results,rect_binned_results=rect_binned_results,iq_binned_results=iq_binned_results))
}


cv_testing_all_ensembles <- function(data,model_types,bin_features_all,nbins_all,cv_K=10){
  cv_index <- cv_cohorts(nrow(data),cv_K)
  results_list <- list(NULL)
  for(fold in 1:cv_K){
    train_data <- data[cv_index!=fold, ]
    test_data <- data[cv_index==fold, ]
    results_list[[fold]] <- testing_all_ensembles(train_data,test_data,model_types,bin_features_all,nbins_all)
  }
  results_list_all <- results_list[[1]]
  cv_weights <- sapply(1:cv_K, function(x) sum(cv_index==x))
  results_list_all[[1]]$accuracy <- sapply(1:nrow(results_list[[1]][[1]]), function(i) weighted.mean(sapply(1:cv_K, function(fold) results_list[[fold]][[1]]$accuracy)[i,],w=cv_weights))
  results_list_all[[2]]$accuracy <- sapply(1:nrow(results_list[[1]][[2]]), function(i) weighted.mean(sapply(1:cv_K, function(fold) results_list[[fold]][[2]]$accuracy)[i,],w=cv_weights))
  results_list_all[[3]]$accuracy <- sapply(1:nrow(results_list[[1]][[3]]), function(i) weighted.mean(sapply(1:cv_K, function(fold) results_list[[fold]][[3]]$accuracy)[i,],w=cv_weights))
  return(results_list_all)
}




