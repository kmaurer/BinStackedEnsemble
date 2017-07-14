#' Exhaustively testing ensemble on new dataset
#'@description Predict using an ensemble classifier created from this package
#'
#'@param ensemble An ensemble classifier built with \code{buildWeightedEnsemble}
#'@param train_data Data to 
#'@param test_data Data to predict with the ensemble
#'
#'@return Predictions for the provided data
#'@export
#'
#'# -------
## Specify combination rules and binning types
weight_type <- "bin weighted"
comb_rule <- "majority vote"
bin_type <- "standard"
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