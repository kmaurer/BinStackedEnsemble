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
  # Loop over treatments and save results
  for(i in 1:nrow(results)){
    weightedEnsemble <- buildWeightedEnsemble(train_data = train_data, model_storage_list = modelList,
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
  # Loop over treatments and save results
  for(i in 1:nrow(results)){
    weightedEnsemble <- buildWeightedEnsemble(train_data = train_data, model_storage_list = modelList,
                                              weightType = results$weight_type[i], comb_rule = results$comb_type[i], 
                                              bin_type = results$bin_type[i], bin_features = bin_features_list[[results$bin_features[i]]],
                                              nbins = nbins_list[[results$nbins[i]]])
    results$accuracy[i] <- sum(predictEnsemble(weightedEnsemble, test_data) == test_data$true_class)/nrow(test_data)
  }
  return(results)
}


iq_binned_testing <- function(train_data, test_data, model_list, bin_features_list, nbins_list ){
  # Make data.frame with all "treatment" combinations
  results <- expand.grid(c("bin weighted","bin dictator"),c("majority vote","average posterior"),c("iterative quantile"),
                         1:length(nbins_list),1:length(bin_features_list))
  names(results) <- c("weight_type","comb_type","bin_type","nbins","bin_features")
  nbins_names <- sapply(nbins_list,function(x) paste(as.character(x), collapse=" X ") )
  bin_features_names <- sapply(bin_features_list,function(x) paste(as.character(x), collapse=" X ") )
  results$bin_pair_name <- bin_features_names[results$bin_features]
  results$nbins_name <-   nbins_names[results$nbins]
  results$accuracy <- NA
  # Loop over treatments and save results
  for(i in 1:nrow(results)){
    weightedEnsemble <- buildWeightedEnsemble(train_data = train_data, model_storage_list = modelList,
                                              weightType = results$weight_type[i], comb_rule = results$comb_type[i], 
                                              bin_type = results$bin_type[i], bin_features = bin_features_list[[results$bin_features[i]]],
                                              nbins = nbins_list[[results$nbins[i]]])
    results$accuracy[i] <- sum(predictEnsemble(weightedEnsemble, test_data) == test_data$true_class)/nrow(test_data)
  }
  return(results)
}


