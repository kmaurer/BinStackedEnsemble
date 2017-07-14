#' Exhaustively testing ensemble on new dataset
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
exhaustive_bin_testing <- function(train_data, test_data, model_list, weight_type, comb_rule, bin_type=NULL, bin_features=NULL, nbins=NULL){
  
  bin_combinations <- expand.grid(weight_type,comb_rule,bin_type,bin_features,)
  ## Make ensemble based on combination/binning type
  ensemble <- buildWeightedEnsemble(train_data, model_list, weight_type, comb_rule, bin_type, bin_features, nbins)
  ## Test it
  ensemble_acc[i,] <- predictEnsemble(ensemble,test_data)
  
}

bin_features <- c("Petal.Length", "Petal.Width")
nbins <- c(1,2,3,4)
make_bin_feature_block <- function(bin_features,nbins){

  bins <- expand.grid(as.data.frame(sapply(bin_features, function(x) nbins)))
  df2 <- as.data.frame(sapply(bin_features, function(x) bin_features))

}
